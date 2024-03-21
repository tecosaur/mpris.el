;;; mpris.el --- Client to the Media Player Remote Interface Specification -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 TEC
;;
;; Author: TEC <contact@tecosaur.net>
;; Maintainer: TEC <contact@tecosaur.net>
;; Created: March 05, 2024
;; Modified: March 05, 2024
;; Version: 0.1.0
;; Keywords:  convenience multimedia tools
;; Homepage: https://code.tecosaur.net/tec/mpris.el
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Client to the Media Player Remote Interface Specification.
;;
;;  Query and control media players over DBus.
;;
;;; Code:

(require 'dbus)
(require 'url)

(defgroup mpris nil
  "Client via the Media Player Remote Interface Specification (MPRIS)."
  :group 'multimedia
  :prefix "mpris-")

(defcustom mpris-preferred-players nil
  "List of preferred media players, in priority order.
Each player should be represended by a string that matches the MPRIS service
name \"org.mpris.MediaPlayer2.Player.*\". Most commonly, this is simply the
application name."
  :type '(repeat string))

(defcustom mpris-disliked-players
  '("kdeconnect" "firefox" "chromium" "plasma-browser-integration")
  "List of disliked media players, only selected when there are no other options.
Each player should be represended by a string that matches the MPRIS service
name \"org.mpris.MediaPlayer2.Player.*\". Most commonly, this is simply the
application name."
  :type '(repeat string))

(defcustom mpris-bus-address :session
  "The DBus address to use.
Should be `:session', `:system', or a string denoting the bus address."
  :type '(choice
          (const :session)
          (const :system)
          string))

(defcustom mpris-current-player-change-hook nil
  "Hooks to run when the current player changes.
Each hook function is called with two arguments: the previous player service and
the new player service."
  :type 'hook)

(defcustom mpris-playback-status-change-hook nil
  "Hooks to run when track metadata changes.
Each hook function is called with two arguments: the service whose playback
status has changed, and the new playback status."
  :type 'hook)

(defcustom mpris-metadata-change-hook nil
  "Hooks to run when track metadata changes.
Each hook function is called with two arguments: the service whose track
metadata has changed, and the new metadata."
  :type 'hook)

(defcustom mpris-current-status-hook nil
  "Hooks to run when anything about the current playback changes.
Each hook function is called with two arguments, the first giving the nature
of the change, the second giving more information on it. This takes one of three
forms:
1. (playback NEW-PLAYBACK-STATUS)
2. (metadata NEW-METADATA)
3. (player NEW-CURRENT-PLAYER-SERVICE)"
  :type 'hook)

(defconst mpris--dbus-path                "/org/mpris/MediaPlayer2")
(defconst mpris--dbus-interface           "org.mpris.MediaPlayer2")
(defconst mpris--dbus-interface-player    "org.mpris.MediaPlayer2.Player")
(defconst mpris--dbus-interface-tracklist "org.mpris.MediaPlayer2.TrackList")
(defconst mpris--dbus-interface-playlists "org.mpris.MediaPlayer2.Playlists")

(defvar mpris-current-player nil
  "The primary DBus service communicated with.")

(defvar mpris--active-players nil
  "List of DBus services for MPRIS-compatible players.")

(defvar mpris--recent-players nil
  "List of players recently selected as the current player.")

(defvar mpris--player-states nil
  "List of information for `mpris--active-players'.
Entries are controlled by `mpris--create-player-watcher' and
`mpris--remove-player-watcher'.

Each entry is of the form:
  \\=(SERVICE-NAME
   :watcher DBUS-WATCHER
   :playback-status STATUS-STRING
   :metadata METADATA-LIST)")

(defvar mpris--is-setup nil
  "Indicator of whether MPRIS is setup up.
Practically, this referres to wheter all the relevant watchers are set up.")

(defvar mpris--dbus-player-existance-watcher nil
  "DBus watcher that looks for new and deleted services.")

(defvar mpris--after-sync-callbacks nil
  "List of funcalls queued to be executed on the next watcher syncronisation.")

(defvar mpris--player-interfaces nil)

;;; Syncronous internal API

(defun mpris--call-method-sync (service interface method args)
  "Call METHOD of the current player's INTERFACE, with ARGS.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (apply #'dbus-call-method
         mpris-bus-address (or service mpris-current-player)
         mpris--dbus-path interface
         method args))

(defun mpris--get-property-sync (service interface property)
  "Get PROPERTY of the current player's INTERFACE.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-get-property
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path interface
   property))

(defun mpris--set-property-sync (service interface property value)
  "Set PROPERTY of the current player's INTERFACE to VALUE.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-set-property
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path interface
   property value))

;;; Async internal API

(defun mpris--call-method-async (handler service interface method args)
  "Asyncronously call METHOD of the current player's INTERFACE, with ARGS.
HANDLER is called on the result of the method call.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (apply #'dbus-call-method-asynchronously
         mpris-bus-address (or service mpris-current-player)
         mpris--dbus-path interface
         method handler args))

(defun mpris--get-property-async (handler service interface property)
  "Asyncronously get PROPERTY of the current player's INTERFACE.
HANDLER is called on the result of the method call.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-call-method-asynchronously
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path dbus-interface-properties
   "Get" handler
   :timeout 500 interface property))

(defun mpris--set-property-async (handler service interface property value)
  "Asyncronously set PROPERTY of the current player's INTERFACE to VALUE.
HANDLER is then called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-call-method-asynchronously
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path dbus-interface-properties
   "Set" handler
   :timeout 500 interface property
   (cons :variant (list value))))

;;; Combined Sync/Async internal API

(defun mpris--call-method (async-handler service interface method &rest args)
  "Call METHOD of the current player's INTERFACE, with ARGS.
If ASYNC-HANDLER is non-nil, this call will be made asyncronously
and ASYNC-HANDLER called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--checked-call
   #'mpris--call-method-sync #'mpris--call-method-async
   async-handler service
   interface method args))

(defun mpris--get-property (async-handler service interface property)
  "Get PROPERTY of the current player's INTERFACE.
If ASYNC-HANDLER is non-nil, this call will be made asyncronously
and ASYNC-HANDLER called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--checked-call
   #'mpris--get-property-sync #'mpris--get-property-async
   async-handler service interface property))

(defun mpris--set-property (async-handler service interface property value)
  "Set PROPERTY of the current player's INTERFACE to VALUE.
If ASYNC-HANDLER is non-nil, this call will be made asyncronously
and ASYNC-HANDLER called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--checked-call
   #'mpris--set-property-sync #'mpris--set-property-async
   async-handler service interface property value))

(defun mpris--checked-call (sync-fn async-fn async-handler service &rest args)
  "Perform a syncronous or asyncronous call, checking the mpris state.
When ASYNC-FN is provided, it will be called like so:
    \\=(apply ASYNC-FN ASYNC-HANDLER SERVICE ARGS)
Otherwise SYNC-FN will be called like so:
    \\=(apply SYNC-FN SERVICE ARGS)"
  (cond
   ((and mpris--is-setup (not service) (not mpris-current-player))
    'no-player)
   ((and mpris--is-setup async-handler)
    (apply async-fn async-handler service args))
   (async-handler
    (push (append (list async-fn async-handler service) args)
          mpris--after-sync-callbacks)
    (mpris--setup-async))
   (mpris--is-setup
    (apply sync-fn service args))
   (service
    (mpris--setup-async)
    (apply sync-fn service args))
   (t
    (mpris--setup-sync)
    (if mpris-current-player
        (apply sync-fn service args)
      'no-player))))

;;; State updates

(defun mpris--setup-async ()
  "Register signal handlers for the current players."
  (mpris--teardown)
  (mpris--list-dbus-services
   (mpris--update-service-list
    #'mpris--setup-1)))

(defun mpris--setup-1 ()
  "Fulfil the promise of `mpris--setup-async'."
  (unless mpris--is-setup
    (setq mpris--is-setup t)
    (unless mpris--dbus-player-existance-watcher
      (setq mpris--dbus-player-existance-watcher
            (dbus-register-signal
             :session
             dbus-interface-dbus
             dbus-path-dbus
             dbus-interface-dbus
             "NameOwnerChanged"
             #'mpris--handle-dbus-name-owner-changed)))
    (mapc #'mpris--create-player-watcher mpris--active-players)
    (unless mpris--active-players
      (mapc (lambda (fn-args) (apply (car fn-args) (cdr fn-args)))
            mpris--after-sync-callbacks)
      (setq mpris--after-sync-callbacks nil))))

(defun mpris--setup-sync ()
  "Perform `mpris--setup-async', but syncronously."
  (mpris--update-service-list
   (dbus-call-method
    mpris-bus-address dbus-service-dbus dbus-path-dbus
    dbus-interface-dbus "ListNames"))
  (mpris--setup-1))

(defun mpris--teardown ()
  "Unregister any watchers created by `mpris--setup-async'."
  (when mpris--dbus-player-existance-watcher
    (dbus-unregister-object mpris--dbus-player-existance-watcher)
    (setq mpris--dbus-player-existance-watcher nil))
  (mapc #'mpris--remove-player-watcher mpris--player-states)
  (setq mpris--is-setup nil))

(defun mpris--create-player-watcher (player-service)
  "Create a watcher for PLAYER-SERVICE and add it to `mpris--player-states'.
Also ensure PLAYER-SERVICE has an entry in `mpris--player-interfaces'."
  (mpris--inspect-interfaces player-service)
  (let ((watcher
         (dbus-register-signal
          :session
          player-service
          mpris--dbus-path
          dbus-interface-properties
          "PropertiesChanged"
          (mpris--create-player-property-change-handler player-service)
          mpris--dbus-interface-player)))
    (push (list player-service :watcher watcher :playback-status nil :metadata nil)
          mpris--player-states)
    (mpris--update-playback-status player-service)
    (mpris--update-metadata player-service)))

(defun mpris--remove-player-watcher (player-service)
  "Cleanly remove PLAYER-SERVICE from `mpris--player-states'."
  (when-let ((state (if (consp player-service) player-service
                      (assoc player-service mpris--player-states))))
    (when (plist-get (cdr state) :watcher)
      (if (member player-service mpris--active-players)
          (dbus-unregister-object (plist-get (cdr state) :watcher))
        ;; When the service itself has been removed, DBus has already removed
        ;; signal watchers, and so we only need to update Emacs' table to
        ;; reflect the new situation.
        (remhash (plist-get (cdr state) :watcher) dbus-registered-objects-table)))
    (setq mpris--player-states (delq state mpris--player-states))))

(defun mpris--create-player-property-change-handler (service)
  "Create a property change handler for SERVICE.
The handler accepts a \"PropertiesChanged\" DBus event, and
modifies `mpris--player-states' appropriately to reflect the new information."
  (lambda (_interface changes _)
    (when-let ((state (cdr (assoc service mpris--player-states))))
      (dolist (change changes)
        (let ((change-type (car-safe change))
              (value (caadr change)))
          (cond
           ((not (stringp change-type)))
           ((and (equal change-type "PlaybackStatus")
                 (stringp value))
            (plist-put state :playback-status value)
            (run-hook-with-args 'mpris-playback-status-change-hook service value)
            (when (equal service mpris-current-player)
              (run-hook-with-args 'mpris-current-status-hook 'playback value))
            (mpris-update-current-player))
           ((and (equal change-type "Metadata")
                 (consp value))
            (plist-put state :metadata value)
            (run-hook-with-args 'mpris-metadata-change-hook service value)
            (when (equal service mpris-current-player)
              (run-hook-with-args 'mpris-current-status-hook 'metadata value)))))))))

(defun mpris--handle-dbus-name-owner-changed (name old-owner new-owner)
  "Handle an DBus ownership change where NAME has gone from OLD-OWNER to NEW-OWNER."
  (when (string-prefix-p "org.mpris.MediaPlayer2." name)
    (cond
     ((string-empty-p old-owner) ; Created service
      (push name mpris--active-players)
      (mpris--create-player-watcher name))
     ((string-empty-p new-owner) ; Deleted service
      (setq mpris--active-players (delete name mpris--active-players))
      (mpris--remove-player-watcher name)
      (when (equal name mpris-current-player)
        (mpris-update-current-player))))))

(defun mpris--update-playback-status (service-name)
  "Update the playback-status entry for SERVICE-NAME in `mpris--player-states'."
  (mpris--get-property-async
   (lambda (status)
     (mpris--update-state-attr service-name :playback-status (car-safe status)))
   service-name
   mpris--dbus-interface-player
   "PlaybackStatus"))

(defun mpris--update-metadata (service-name)
  "Update the metadata entry for SERVICE-NAME in `mpris--player-states'."
  (mpris--get-property-async
   (lambda (metadata)
     (mpris--update-state-attr service-name :metadata (car-safe metadata)))
   service-name
   mpris--dbus-interface-player
   "Metadata"))

(defun mpris--update-state-attr (service-name attr value)
  "Set SERVICE-NAME's recorded ATTR to VALUE in `mpris--player-states'."
  (let (incomplete did-initialise-attr)
    (dolist (state mpris--player-states)
      (when (equal (car state) service-name)
        (unless (plist-get (cdr state) attr)
          (setq did-initialise-attr t))
        (plist-put (cdr state) attr value))
      (unless incomplete
        (when (memq nil (cdr state))
          (setq incomplete t))))
    ;; If `did-initialise-attr' and not `incomplete', this means we've just
    ;; finished synchronising `mpris--player-states' (likely after adding
    ;; a new player). As such, it makes sense to update the current player
    ;; and run the after-sync callbacks.
    (when (and did-initialise-attr (not incomplete))
      (mpris-update-current-player)
      (mapc (lambda (fn-args) (apply (car fn-args) (cdr fn-args)))
            mpris--after-sync-callbacks)
      (setq mpris--after-sync-callbacks nil))))

(defun mpris--list-dbus-services (callback)
  "Fetch a list of DBus services, then call CALLBACK on it."
  (dbus-call-method-asynchronously
   mpris-bus-address dbus-service-dbus dbus-path-dbus
   dbus-interface-dbus "ListNames" callback))

(defun mpris--update-service-list (service-list-or-callback &rest args)
  "Update the service list, possible running a callback in the process.

SERVICE-LIST-OR-CALLBACK can either be:
1. A list of services, as given by `mpris--list-dbus-services'.
   This function then filters them to MPRIS services and updates
   `mpris--active-players'.
2. A callback function, in which case a closure will be generated that
   when invoked performs (1) and then also invokes the callback with ARGS."
  (cond
   ((functionp service-list-or-callback)
    (lambda (service-list)
      (mpris--update-service-list service-list)
      (apply service-list-or-callback args)))
   ((consp service-list-or-callback)
    (setq mpris--active-players nil)
    (dolist (service service-list-or-callback)
      (when (string-prefix-p "org.mpris.MediaPlayer2." service)
        (push service mpris--active-players))))))

(defun mpris--inspect-interfaces (service)
  "Populate `mpris--player-interfaces' with information on SERVICE's interfaces."
  (unless (assoc service mpris--player-interfaces)
    (let (data)
      (dolist (interface (dbus-introspect-get-interface-names
                          mpris-bus-address service mpris--dbus-path))
        (let ((methods (dbus-introspect-get-method-names
                        mpris-bus-address service mpris--dbus-path interface))
              (properties (dbus-introspect-get-property-names
                           mpris-bus-address service mpris--dbus-path interface)))
          (push (list interface methods properties) data)))
      (push (cons service data) mpris--player-interfaces))))

(defun mpris--player-interface-p (service interface)
  "Return non-nil if SERVICE implements INTERFACE."
  (and (assoc interface (cdr (assoc service mpris--player-interfaces))) t))

(defun mpris--player-method-p (service interface method)
  "Return non-nil if SERVICE implements INTERFACE and METHOD."
  (and (member method (cadr (assoc interface (cdr (assoc service mpris--player-interfaces))))) t))

(defun mpris--player-property-p (service interface property)
  "Return non-nil if SERVICE implements INTERFACE and has PROPERTY."
  (and (member property (caddr (assoc interface (cdr (assoc service mpris--player-interfaces))))) t))

;;; The current player

(defun mpris-update-current-player ()
  "Check and update the player currently being interfaced with.
This relies on active DBus watchers maintaining `mpris--active-players'
and `mpris--player-states'."
  ;; If the current player exists and is playing, do nothing.
  (unless (and (member mpris-current-player mpris--active-players)
               (equal "Playing" (mpris-get-playback-status))
               (let (disliked-player-p)
                 (dolist (player mpris-disliked-players)
                   (when (string-prefix-p (concat "org.mpris.MediaPlayer2." player) mpris-current-player)
                     (setq disliked-player-p t)))
                 (not disliked-player-p)))
    (let ((candidate-services mpris--active-players)
          (previous-service mpris-current-player)
          (service-sorter (lambda (a b) (> (cdr a) (cdr b))))
          preferred-services recent-services common-services disliked-services
          selected-service)
      ;; Process the candidate services into seperate priority lists.
      (dolist (service candidate-services)
        (let (name)
          (dotimes (o (- (length service) 23))
            (when (and (not name) (= (aref service (+ 23 o)) ?.))
              (setq name (substring service 23 (+ 23 o)))))
          (unless name
            (setq name (substring service 23)))
          (cond
           ((member service mpris--recent-players)
            (push (cons service (length (member service mpris--recent-players)))
                  recent-services))
           ((member name mpris-disliked-players)
            (push (cons service (- (length (member name mpris-disliked-players))))
                  disliked-services))
           ((member name mpris-preferred-players)
            (push (cons service (length (member name mpris-preferred-players)))
                  preferred-services))
           (t (push service common-services)))))
      ;; This next bit might look weird, but we actually want the ordering of
      ;; `mpris-disliked-players' and `mpris-preferred-players' to matter, and
      (setq recent-services    (mapcar #'car (sort recent-services service-sorter)))
      (setq preferred-services (mapcar #'car (sort preferred-services service-sorter)))
      (setq disliked-services  (mapcar #'car (sort disliked-services service-sorter)))
      ;; First, look for playing services. With priority order:
      ;; 1. Preferred
      ;; 2. Recently played
      ;; 3. Remaining (not disliked)
      ;; 4. Disliked services
      ;; Then try again with paused services.
      (dolist (playback-state '("Playing" "Paused"))
        (dolist (service-sublist
                 (list recent-services preferred-services common-services disliked-services))
          (dolist (service service-sublist)
            (unless selected-service
              (when (equal playback-state (mpris-get-playback-status nil service))
                (setq selected-service service))))))
      ;; If no service has been selected so far, apply the same priority order
      ;; but now just looking for any service that exists.
      (unless selected-service
        (setq selected-service
              (or (car recent-services)
                  (car preferred-services)
                  (car common-services)
                  (car disliked-services))))
      ;; Now do what we promised and set the current player.
      (setq mpris-current-player selected-service)
      ;; If we've selected a different player to the current one, a few things need updating.
      (unless (equal previous-service selected-service)
        (when (equal "Playing" (mpris-get-playback-status nil selected-service))
          (setq mpris--recent-players (delete selected-service mpris--recent-players))
          (push selected-service mpris--recent-players))
        (run-hook-with-args 'mpris-current-player-change-hook
                            previous-service selected-service)
        (run-hook-with-args 'mpris-current-status-hook 'player selected-service)))))

(defun mpris--check-current-player (&optional callback-fn &rest callback-args)
  "Return non-nil if a player currently exists.
If no player exists, return nil and looks for a player in the background.

Optionally, CALLBACK-FN and CALLBACK-ARGS can be provided. The
callback is either invoked immediately if a current player
exists, or after the background player update completes."
  (cond
   ((and mpris-current-player callback-fn)
    (apply callback-fn callback-args))
   (mpris-current-player t)
   ((and callback-fn (not mpris--dbus-player-existance-watcher))
    (push (cons callback-fn callback-args) mpris--after-sync-callbacks)
    (mpris--setup-async))))

;;; MPRIS API
;; Here we define all the method calls and property acessors
;; listed in <https://specifications.freedesktop.org/mpris-spec/2.2/Media_Player.html>

;;;; MPRIS API - Methods

(defun mpris-raise (&optional async-handler service)
  "Bring the current player's user interface to the front, if possible.

The media player may be unable to control how its user interface
is displayed, or it may not have a graphical user interface at
all. In this case, the `mpris-can-raise' property is false and
this method does nothing.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--call-method async-handler service mpris--dbus-interface "Raise"))

(defun mpris-quit (&optional async-handler service)
  "Stop running the current player.

The media player may refuse to allow clients to shut it down. In
this case, the `mpris-can-quit' property is false and this method
does nothing.

Note: Media players which can be D-Bus activated, or for which
there is no sensibly easy way to terminate a running
instance (via the main interface or a notification area icon for
example) should allow clients to use this method. Otherwise, it
should not be needed.

If the media player does not have a UI, this should be implemented.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--call-method async-handler service mpris--dbus-interface "Quit"))

;;;; MPRIS API - Properties

(defun mpris-can-quit (&optional async-handler service)
  "Whether quit is expected to have an effect.

If false, calling `mpris-quit' will have no effect, and may raise
a NotSupported error. If true, calling `mpris-quit' will cause
the media application to attempt to quit (although it may still
be prevented from quitting by the user, for example).

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                      mpris--dbus-interface "CanQuit"))

(defun mpris-get-fullscreen (&optional async-handler service)
  "Whether the current player is occupying the fullscreen.

This is typically used for videos. A value of true indicates that
the media player is taking up the full screen.

Media centre software may well have this value fixed to true

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-property-p (or service mpris-current-player) mpris--dbus-interface "Fullscreen")
      (mpris--get-property async-handler service mpris--dbus-interface "Fullscreen")
    'unimplemented))

(defun mpris-set-fullscreen (value &optional async-handler service)
  "Set the current player fullscreen status to VALUE (boolean).

If `mpris-can-set-fullscreen' is true, clients may set this
property to true to tell the media player to enter fullscreen
mode, or to false to return to windowed mode.

If `mpris-can-set-fullscreen' is false, then attempting to set
this property should have no effect, and may raise an error.
However, even if it is true, the media player may still be unable
to fulfil the request, in which case attempting to set this
property will have no effect (but should not raise an error).

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-property-p (or service mpris-current-player) mpris--dbus-interface "Fullscreen")
      (mpris--set-property async-handler service mpris--dbus-interface
                           "Fullscreen" value)
    'unimplemented))

(defun mpris-can-set-fullscreen (&optional async-handler service)
  "Whether the current player's fullscreen state can be controlled.

If false, attempting `mpris-set-fullscreen' will have no effect,
and may raise an error. If true, attempting
`mpris-set-fullscreen' will not raise an error, and (if it is
different from the current value) will cause the media player to
attempt to enter or exit fullscreen mode.

Note that the media player may be unable to fulfil the request.
In this case, the value will not change. If the media player
knows in advance that it will not be able to fulfil the request,
however, this property should be false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-property-p
       (or service mpris-current-player) mpris--dbus-interface "CanSetFullscreen")
      (mpris--get-property async-handler service mpris--dbus-interface "CanSetFullscreen")
    'unimplemented))

(defun mpris-can-raise (&optional async-handler service)
  "Whether the current player can be raised.

If false, calling `mpris-raise' will have no effect, and may
raise a NotSupported error. If true, calling `mpris-raise' will
cause the media application to attempt to bring its user
interface to the front, although it may be prevented from doing
so (by the window manager, for example).

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                      mpris--dbus-interface "CanRaise"))

(defun mpris-has-track-list (&optional async-handler service)
  "Indicate whether the current player implements the TrackList interface.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                      mpris--dbus-interface "HasTrackList"))

(defun mpris-identity (&optional async-handler service)
  "A friendly name to identify the current player to users.

This should usually match the name found in .desktop files.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                      mpris--dbus-interface "Identity"))

(defun mpris-desktop-entry (&optional async-handler service)
  "The desktop entry of the current player.

The basename of an installed .desktop file which complies with
the Desktop entry specification, with the \".desktop\" extension stripped.

Example: The desktop entry file is
\"/usr/share/applications/vlc.desktop\", and this property
contains \"vlc\".

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                      mpris--dbus-interface "DesktopEntry"))

(defun mpris-supported-uri-schemes (&optional async-handler service)
  "The URI schemes supported by the current player.

This can be viewed as protocols supported by the player in almost
all cases. Almost every media player will include support for the
\"file\" scheme. Other common schemes are \"http\" and \"rtsp\".

Note that URI schemes should be lower-case.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                      mpris--dbus-interface "SupportedUriSchemes"))

(defun mpris-supported-mime-types (&optional async-handler service)
  "The mime-types supported by the current player.

Mime-types should be in the standard format (eg: \"audio/mpeg\"
or \"application/ogg\").

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                      mpris--dbus-interface "SupportedMimeTypes"))

;;; MPRIS Player API
;; Here we define all the method calls and property acessors
;; listed in <https://specifications.freedesktop.org/mpris-spec/latest/Player_Interface.html>

;;;; MPRIS Player API - Methods

(defun mpris-next (&optional async-handler service)
  "Skip to the next track in the tracklist.

If there is no next track (and endless playback and track repeat are both off),
stop playback. If playback is paused or stopped, it remains that way.

If `mpris-can-go-next' is false, attempting to call this method
should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service mpris--dbus-interface-player "Next"))

(defun mpris-previous (&optional async-handler service)
  "Skips to the previous track in the tracklist.

If there is no previous track (and endless playback and track repeat are both
off), stop playback.  If playback is paused or stopped, it remains that way.

If `mpris-can-go-previous' is false, attempting to call this
method should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service mpris--dbus-interface-player "Previous"))

(defun mpris-pause (&optional async-handler service)
  "Pauses playback.

If playback is already paused, this has no effect. Calling Play after this
should cause playback to start again from the same position.

If `mpris-can-pause' is false, attempting to call this method
should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service mpris--dbus-interface-player "Pause"))

(defun mpris-play-pause (&optional async-handler service)
  "Pause or resume playback.

If playback is already paused, resumes playback. If playback is stopped, starts
playback.

If `mpris-can-pause' is false, attempting to call this method
should have no effect and raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service mpris--dbus-interface-player "PlayPause"))

(defun mpris-stop (&optional async-handler service)
  "Stops playback.

If playback is already stopped, this has no effect. Calling Play after this
should cause playback to start again from the beginning of the track.

If `mpris-can-control' is false, attempting to call this method
should have no effect and raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service mpris--dbus-interface-player "Stop"))

(defun mpris-play (&optional async-handler service)
  "Start or resume playback.

If already playing, this has no effect.
If paused, playback resumes from the current position.
If there is no track to play, this has no effect.

If `mpris-can-play' is false, attempting to call this method
should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service mpris--dbus-interface-player "Play"))

(defun mpris-seek (offset &optional async-handler service)
  "Seeks forward in the current track by OFFSET microseconds.

A negative value seeks back. If this would mean seeking back further than the
start of the track, the position is set to 0.  If the value passed in would mean
seeking beyond the end of the track, acts like a call to Next.

If the `mpris-can-seek' property is false, this has no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--call-method async-handler service mpris--dbus-interface-player
                      "Seek" :int64 offset))

(defun mpris-set-position (track-id position &optional async-handler service)
  "Set the current track (TRACK-ID) playback to POSITION microseconds.

If POSITION argument is less than 0, do nothing.
If POSITION argument is greater than the track length, do nothing.

If the `mpris-can-seek' property is false, this has no effect.

The TRACK-ID argument is required to avoid race conditions where
a client tries to seek a position after the track has already
been changed.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--call-method async-handler service mpris--dbus-interface-player
                      "SetPosition" :object-path track-id :int64 position))

(defun mpris-open-uri (uri &optional async-handler service)
  "Open URI with the current player.

The uri scheme should be an element of
`mpris-supported-uri-schemes' and the mime-type should match one
of the elements of the `mpris-supported-mime-types'.

If the uri scheme or the mime-type of the uri to open is not
supported, this method does nothing and may raise an error. In
particular, if the list of available uri schemes is empty, this
method may not be implemented.

Clients should not assume that URI has been opened as soon as
this method returns. They should wait until the mpris:trackid
field in the Metadata property changes.

If the media player implements the TrackList interface, then the
opened track should be made part of the tracklist, the
org.mpris.MediaPlayer2.TrackList.TrackAdded or
org.mpris.MediaPlayer2.TrackList.TrackListReplaced signal should
be fired, as well as the
org.freedesktop.DBus.Properties.PropertiesChanged signal on the
tracklist interface.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--call-method async-handler service mpris--dbus-interface-player
                      "OpenUri" uri))

;;; MPRIS Player API - Properties

(defun mpris-get-playback-status (&optional async-handler service)
  "Get the current playback status.

Either \"Playing\", \"Paused\", or \"Stopped\".

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if-let ((value
            (plist-get
             (cdr (assoc (or service mpris-current-player) mpris--player-states))
             :playback-status)))
      (if async-handler
          (funcall async-handler value)
        value)
    (mpris--get-property async-handler service
                         mpris--dbus-interface-player "PlaybackStatus")))

(defun mpris-get-loop-status (&optional async-handler service)
  "Get the current loop/repeat status.

May be:
- \"None\" if the playback will stop when there are no more tracks to play.
- \"Track\" if the current track will start again from the begining once it has
  finished playing.
- \"Playlist\" if the playback loops through a list of tracks.

This is an *optional* part of the MPRIS2 specification, if the current player
does not support this option the symbol \\='unimplemented will be returned.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-property-p
       (or service mpris-current-player) mpris--dbus-interface-player "LoopStatus")
      (mpris--get-property async-handler service mpris--dbus-interface-player
                           "LoopStatus")
    'unimplemented))

(defun mpris-set-loop-status (value &optional async-handler service)
  "Set the current loop/repeat status to VALUE.

VALUE may be:
- \"None\" the playback will stop when there are no more tracks to play.
- \"Track\" the current track will start again from the begining once it has
  finished playing.
- \"Playlist\" the playback loops through a list of tracks.

This is an *optional* part of the MPRIS2 specification, if the current player
does not support this option the symbol \\='unimplemented will be returned.

If `mpris-can-control' is false, attempting to set this property
should have no effect and raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-property-p
       (or service mpris-current-player) mpris--dbus-interface-player "LoopStatus")
      (mpris--set-property async-handler service mpris--dbus-interface-player
                           "LoopStatus" value)
    'unimplemented))

(defun mpris-get-rate (&optional async-handler service)
  "Get the current playback rate.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "Rate"))

(defun mpris-set-rate (value &optional async-handler service)
  "Set the current playback rate to VALUE.

The value must fall in the range described by `mpris-get-minimum-rate' and
MaximumRate, and must not be 0.0. If playback is paused, the
PlaybackStatus property should be used to indicate this. A value
of 0.0 should not be set by the client. If it is, the media
player should act as though `mpris-pause' was called.

If the media player has no ability to play at speeds other than
the normal playback rate, this must still be implemented, and
must return 1.0. The `mpris-get-minimum-rate' and
`mpris-get-maximum-rate' properties must also be set to 1.0.

Not all values may be accepted by the media player. It is left to
media player implementations to decide how to deal with values
they cannot use; they may either ignore them or pick a best fit
value. Clients are recommended to only use sensible fractions or
multiples of 1 (eg: 0.5, 0.25, 1.5, 2.0, etc).

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--set-property async-handler service mpris--dbus-interface-player
                       "Rate" value))

(defun mpris-get-shuffle (&optional async-handler service)
  "Whether the playlist is progressing in a non-linear order.

This is an *optional* part of the MPRIS2 specification, if the current player
does not support this option the symbol \\='unimplemented will be returned.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-property-p
       (or service mpris-current-player) mpris--dbus-interface-player "Shuffle")
      (mpris--get-property async-handler service mpris--dbus-interface-player
                           "Shuffle")
    'unimplemented))

(defun mpris-set-shuffle (value &optional async-handler service)
  "Set the playlist to progress in a non-linear order depending on VALUE.

VALUE should be either t or nil.

This is an *optional* part of the MPRIS2 specification, if the current player
does not support this option the symbol \\='unimplemented will be returned.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-property-p
       (or service mpris-current-player) mpris--dbus-interface-player "Shuffle")
      (mpris--set-property async-handler service mpris--dbus-interface-player
                           "Shuffle" value)
    'unimplemented))

(defun mpris-get-metadata (&optional async-handler service)
  "Get the metadata of the current track.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if-let ((value
            (plist-get
             (cdr (assoc (or service mpris-current-player) mpris--player-states))
             :metadata)))
      (if async-handler
          (funcall async-handler value)
        value)
    (mpris--get-property async-handler service
                         mpris--dbus-interface-player "Metadata")))

(defun mpris-get-volume (&optional async-handler service)
  "Get the volume level.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "Volume"))

(defun mpris-set-volume (value &optional async-handler service)
  "Set the volume level to VALUE.

When setting, if a negative value is passed, the volume should be
set to 0.0.

If `mpris-can-control' is false, attempting to set this property
should have no effect and raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--set-property async-handler service mpris--dbus-interface-player
                       "Volume" value))

(defun mpris-get-position (&optional async-handler service)
  "The current track position in microseconds.

The position is between 0 and the \"mpris:length\" metadata
entry (see Metadata).

Note: If the media player allows it, the current playback
position can be changed with either `mpris-set-position' or
`mpris-seek'. If this is not the case, the `mpris-can-seek'
property is false, and setting this property has no effect and
can raise an error.

If the playback progresses in a way that is inconstistant with
the Rate property (`mpris-get-rate'), the Seeked signal is
emited.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "Position"))

(defun mpris-get-minimum-rate (&optional async-handler service)
  "The minimum value which the Rate property can take.

Clients should not call `mpris-set-rate' with a lower value.

Note that even if this value is 0.0 or negative, `mpris-set-rate'
should not be called with a value of 0.0 or below.

This value should always be 1.0 or less.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "MinimumRate"))

(defun mpris-get-maximum-rate (&optional async-handler service)
  "The maximum value which the Rate property can take.

Clients should not call `mpris-set-rate' with a greater value.

This value should always be 1.0 or greater.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "MaximumRate"))

(defun mpris-can-go-next (&optional async-handler service)
  "Whether `mpris-next' is expected to change the current track.

More specifically, whether the client can call `mpris-next' and
expect the current track to change.

If it is unknown whether a `mpris-next' call will be
successful (for example, when streaming tracks), this property
should be set to true.

If `mpris-can-control' is false, this property should also be
false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "CanGoNext"))

(defun mpris-can-go-previous (&optional async-handler service)
  "Whether `mpris-previous' is expected to change the current track.

More specifically, whether the client can call `mpris-previous'
and expect the current track to change.

If it is unknown whether a `mpris-previous' call will be
successful (for example, when streaming tracks), this property
should be set to true.

If `mpris-can-control' is false, this property should also be false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "CanGoPrevious"))

(defun mpris-can-play (&optional async-handler service)
  "Whether playback can be started using `mpris-play' or `mpris-play-pause'.

Note that this is related to whether there is a \"current
track\": the value should not depend on whether the track is
currently paused or playing. In fact, if a track is currently
playing (and `mpris-can-control' is true), this should be true.

If `mpris-can-control' is false, this property should also be
false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "CanPlay"))

(defun mpris-can-pause (&optional async-handler service)
  "Whether playback can be paused using `mpris-pause' or `mpris-play-pause'.

Note that this is an intrinsic property of the current track: its
value should not depend on whether the track is currently paused
or playing. In fact, if playback is currently paused (and
CanControl is true), this should be true.

If `mpris-can-control' is false, this property should also be
false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler (or service mpris-current-player)
                       mpris--dbus-interface-player "CanPause"))

(defun mpris-can-seek (&optional async-handler service)
  "Whether `mpris-seek' and `mpris-set-position' can control the playback position.

This may be different for different tracks.

If `mpris-can-control' is false, this property should also be
false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "CanSeek"))

(defun mpris-can-control (&optional async-handler service)
  "Whether the media player may be controlled over this interface.

This property is not expected to change, as it describes an
intrinsic capability of the implementation.

If this is false, clients should assume that all properties on
this interface are read-only (and will raise errors if writing to
them is attempted), no methods are implemented and all other
properties starting with \"Can\" are also false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property async-handler service
                       mpris--dbus-interface-player "CanControl"))

(defun mpris-open-file (file &optional async-handler service)
  "Open FILE with the current player.

A thin wrapper around `mpris-open-uri'.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris-open-uri
   (url-encode-url (mpris--file-name-uri file))
   async-handler service))

;;; MPRIS TrackList API
;; Here we define all the method calls and property acessors
;; listed in <https://specifications.freedesktop.org/mpris-spec/latest/Track_List_Interface.html>

;;;; MPRIS TrackList API - Methods

(defun mpris-get-tracks-metadata (track-ids &optional async-handler service)
  "Gets all the metadata available for a set of tracks.

TRACK-IDS is the list of track ids for which metadata is requested.

Each set of metadata must have a \"mpris:trackid\" entry at the
very least, which contains a string that uniquely identifies this
track within the scope of the tracklist.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-tracklist)
      (mpris--call-method async-handler service mpris--dbus-interface-tracklist
                          "GetTracksMetadata" '(:array :object-path) track-ids)
    'unimplemented))

(defun mpris-add-track (uri after-track-id set-as-current &optional async-handler service)
  "Add a URI to the TrackList.

- URI is the uri of the item to add. Its uri scheme should be an
  element of the `mpris-supported-uri-schemes' property and the
  mime-type should match one of the elements of
  `mpris-supported-mime-types'.

- AFTER-TRACK-ID is the identifier of the track after which the
  new item should be inserted. The path
  \"/org/mpris/MediaPlayer2/TrackList/NoTrack\" indicates that
  the track should be inserted at the start of the track list.

- SET-AS-CURRENT indicates whether the newly inserted track
  should be considered as the current track. Setting this to true
  has the same effect as calling GoTo afterwards.

If `mpris-can-edit-tracks' is false, this has no effect.

Note: Clients should not assume that the track has been added at
the time when this method returns. They should wait for a
TrackAdded (or TrackListReplaced) signal.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-tracklist)
      (mpris--call-method async-handler service mpris--dbus-interface-tracklist
                          "AddTrack" :string uri :object-path after-track-id :boolean set-as-current)
    'unimplemented))

(defun mpris-remove-track (track-id &optional async-handler service)
  "Remove an item from the TrackList.

- TRACK-ID is the identifier of the track to be removed.
  \"/org/mpris/MediaPlayer2/TrackList/NoTrack\" is not a valid
  value for this argument.

If the track is not part of this tracklist, this has no effect.

If the `mpris-can-edit-tracks' property is false, this has no effect.

Note: Clients should not assume that the track has been removed
at the time when this method returns. They should wait for a
TrackRemoved (or TrackListReplaced) signal.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-tracklist)
      (mpris--call-method async-handler service mpris--dbus-interface-tracklist
                          "RemoveTrack" :object-path track-id)
    'unimplemented))

(defun mpris-go-to-track (track-id &optional async-handler service)
  "Skip to the specified TRACK-ID.

- TRACK-ID is the identifier of the track to skip to.
  \"/org/mpris/MediaPlayer2/TrackList/NoTrack\" is not a valid
  value for this argument.

If the track is not part of this tracklist, this has no effect.

If this object is not \"/org/mpris/MediaPlayer2\", the current
TrackList's tracks should be replaced with the contents of this
TrackList, and the TrackListReplaced signal should be fired from
\"/org/mpris/MediaPlayer2\".

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-tracklist)
      (mpris--call-method async-handler service mpris--dbus-interface-tracklist
                          "GoTo" :object-path track-id)
    'unimplemented))

;;;; MPRIS TrackList API - Properties

(defun mpris-get-tracks (&optional async-handler service)
  "Get each track in the current tracklist, in order.

The org.freedesktop.DBus.Properties.PropertiesChanged signal is
emited every time this property changes, but the signal message
does not contain the new value. Client implementations should
rather rely on the TrackAdded, TrackRemoved and TrackListReplaced
signals to keep their representation of the tracklist up to date.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-tracklist)
      (mpris--get-property async-handler service mpris--dbus-interface-tracklist "Tracks")
    'unimplemented))

(defun mpris-can-edit-tracks (&optional async-handler service)
  "Whether the track list can be edited.

If false, calling `mpris-add-track' or `mpris-remove-track' will
have no effect, and may raise a NotSupported error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-tracklist)
      (mpris--get-property async-handler service mpris--dbus-interface-tracklist "")
    'unimplemented))

;;; MPRIS Playlists API
;; Here we define all the method calls and property acessors
;; listed in <https://specifications.freedesktop.org/mpris-spec/latest/Playlists_Interface.html>

;;;; MPRIS Playlists API - Methods

(defun mpris-activate-playlist (playlist-id &optional async-handler service)
  "Start playing PLAYLIST-ID.

It is up to the media player whether this completely replaces the
current tracklist, or whether it is merely inserted into the
tracklist and the first track starts. For example, if the media
player is operating in a \"jukebox\" mode, it may just append the
playlist to the list of upcoming tracks, and skip to the first
track in the playlist.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-playlists)
      (mpris--call-method async-handler service mpris--dbus-interface-playlists
                          "ActivatePlaylist" :object-path playlist-id)
    'unimplemented))

(defun mpris-get-playlists (index max-count order reverse-order &optional async-handler service)
  "Get a set of playlists.

- INDEX is the integer index of the first playlist to be
  fetched (according to the ordering).

- MAX-COUNT is the maximum number of playlists to fetch.

- ORDER specifies the ordering that should be used, it should be one of
  `mpris-get-orderings', usually:
  - \"Alphabetical\"
  - \"CreationDate\"
  - \"ModifiedDate\"
  - \"LastPlayDate\"
  - \"UserDefined\"

- REVERSE-ORDER specifies whether the order should be reversed.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-playlists)
      (mpris--call-method async-handler service mpris--dbus-interface-playlists
                          "GetPlaylists" :int64 index :int64 max-count order :boolean reverse-order)
    'unimplemented))

;;;; MPRIS Playlists API - Properties

(defun mpris-get-playlist-count (&optional async-handler service)
  "The number of playlists available.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-playlists)
      (mpris--get-property async-handler service mpris--dbus-interface-playlists
                           "PlaylistCount")
    'unimplemented))

(defun mpris-get-orderings (&optional async-handler service)
  "The availible orderings.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-playlists)
      (mpris--get-property async-handler service mpris--dbus-interface-playlists
                           "Orderings")
    'unimplemented))

(defun mpris-active-playlist (&optional async-handler service)
  "The currently-active playlist.

If there is no currently-active playlist, the structure's Valid
field will be false, and the Playlist details are undefined.

Note that this may not have a value even after ActivatePlaylist
is called with a valid playlist id as ActivatePlaylist
implementations have the option of simply inserting the contents
of the playlist into the current tracklist.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if (mpris--player-interface-p (or service mpris-current-player) mpris--dbus-interface-playlists)
      (mpris--get-property async-handler service mpris--dbus-interface-playlists
                           "ActivePlaylist")
    'unimplemented))

;;; MPRIS Utility functions

(defun mpris-track-attr (attr &optional service)
  "Attempt to determine ATTR of the current track.

ATTR should either be a string correspanding to a metadata entry, or a symbol
from the following list:
- trackid
- title
- album
- artist (or artists)
- album-artist
- length
- genre
- url or file
- rating
- art-url or art-file

To use a specific player, set SERVICE to the target MediaPlayer2 service."
  (let ((key
         (pcase attr
           ('trackid "mpris:trackid")
           ('title "xesam:title")
           ('album "xesam:album")
           ((or 'artist 'artists) "xesam:artist")
           ('album-artist "xesam:albumArtist")
           ('length "mpris:length")
           ('genre "xesam:genre")
           ((or 'url 'file) "xesam:url")
           ('rating "xesam:userRating")
           ((or 'art-url 'art-file)
            "mpris:artUrl")
           ((pred stringp) attr)
           (_ (error "Unrecognised attribute: %s" attr))))
        (value-extractor
         (pcase attr
           ((or 'artist 'album-artist) #'caar)
           ((or 'file 'art-file)
            (lambda (v)
              (and (car v)
                   (mpris--uri-to-file (car v)))))
           (_ #'car)))
        (metadata (mpris-get-metadata nil service)))
    (and (not (eq metadata 'no-player))
         (funcall value-extractor (cadr (assoc key metadata))))))

(defun mpris--uri-to-file (uri)
  "Try to get a file path for the content referred to by URI."
  (if (string-prefix-p "file://" uri)
      (url-filename (url-generic-parse-url uri))
    (let ((temp-file
           (file-name-concat
            temporary-file-directory
            (file-name-with-extension
             (concat "emacs-mpris-" (sha1 uri))
             (or (file-name-extension uri) "img")))))
      (if (file-exists-p temp-file)
          temp-file
        (and (url-copy-file uri temp-file)
             temp-file)))))

(defun mpris--file-name-uri (f)
  "Return a URI for the filename F.
Copy of `rng-file-name-uri'."
  (setq f (expand-file-name f))
  (let ((uri
         (replace-regexp-in-string "[]\0-\s\"#%;<>?[\\^`{|}\177]"
                                   #'mpris--percent-encode f)))
    (concat "file:"
            (if (and (> (length uri) 0)
                     (= (aref uri 0) ?/))
                "//"
              "///")
            uri)))

(defun mpris--percent-encode (str)
  "Percent encode every char in STR.
Used in `mpris--file-name-uri'"
  (apply #'concat
         (mapcar (lambda (ch)
                   (format "%%%x%x" (/ ch 16) (% ch 16)))
                 (string-to-list str))))

(provide 'mpris)
;;; mpris.el ends here
