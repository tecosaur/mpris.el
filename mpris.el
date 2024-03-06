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

(defconst mpris--dbus-path "/org/mpris/MediaPlayer2")
(defconst mpris--dbus-interface "org.mpris.MediaPlayer2.Player")

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

(defvar mpris--dbus-player-existance-watcher nil
  "DBus watcher that looks for new and deleted services.")

(defvar mpris--after-init-callbacks nil
  "List of funcalls queued to be executed after watcher initialisation.")

;;; Syncronous internal API

(defun mpris--call-method-sync (method args &optional service)
  "Call METHOD of the current player, with ARGS.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (apply #'dbus-call-method
         mpris-bus-address (or service mpris-current-player)
         mpris--dbus-path mpris--dbus-interface
         method args))

(defun mpris--get-property-sync (property &optional service)
  "Asyncronously get PROPERTY of the current player, call HANDLER on the result.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-get-property
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path mpris--dbus-interface property))

(defun mpris--set-property-sync (property value &optional service)
  "Asyncronously set PROPERTY of the current player to VALUE.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-set-property
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path mpris--dbus-interface property value))

;;; Async internal API

(defun mpris--call-method-async (method args handler &optional service)
  "Asyncronously call METHOD of the current player, with ARGS.
HANDLER is run on the result of the method call.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (apply #'dbus-call-method-asynchronously
         mpris-bus-address (or service mpris-current-player)
         mpris--dbus-path mpris--dbus-interface
         method handler args))

(defun mpris--get-property-async (property handler &optional service)
  "Asyncronously get PROPERTY of the current player, call HANDLER on the result.
To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-call-method-asynchronously
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path dbus-interface-properties
   "Get" handler
   :timeout 500 mpris--dbus-interface property))

(defun mpris--set-property-async (property value handler &optional service)
  "Asyncronously set PROPERTY of the current player to VALUE.
HANDLER is then called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service."
  (dbus-call-method-asynchronously
   mpris-bus-address (or service mpris-current-player)
   mpris--dbus-path dbus-interface-properties
   "Set" handler
   :timeout 500 mpris--dbus-interface property
   (cons :variant (list value))))

;;; Combined Sync/Async internal API

(defun mpris--call-method (async-handler service method &rest args)
  "Call METHOD of the current player, with ARGS.
If ASYNC-HANDLER is non-nil, this call will be made asyncronously
and ASYNC-HANDLER called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (cond
   ((and async-handler service)
    (mpris--call-method-async method args async-handler service))
   (async-handler
    (mpris--check-current-player
     #'mpris--call-method-async method args async-handler service))
   ((or service (mpris--check-current-player))
    (mpris--call-method-sync method args service))
   (t
    (unless mpris--dbus-player-existance-watcher
      (mpris--setup-watchers))
    'no-player)))

(defun mpris--get-property (property &optional async-handler service)
  "Get PROPERTY of the current player.
If ASYNC-HANDLER is non-nil, this call will be made asyncronously
and ASYNC-HANDLER called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (cond
   ((and async-handler service)
    (mpris--get-property-async property async-handler service))
   (async-handler
    (mpris--check-current-player
     #'mpris--get-property-async property async-handler service))
   ((or service (mpris--check-current-player))
    (mpris--get-property-sync property service))
   (t
    (unless mpris--dbus-player-existance-watcher
      (mpris--setup-watchers))
    'no-player)))

(defun mpris--set-property (property value &optional async-handler service)
  "Set PROPERTY of the current player to VALUE.
If ASYNC-HANDLER is non-nil, this call will be made asyncronously
and ASYNC-HANDLER called on the result.

To use a different player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (cond
   ((and async-handler service)
    (mpris--set-property-async property value async-handler service))
   (async-handler
    (mpris--check-current-player
     #'mpris--set-property-async property value async-handler service))
   ((or service (mpris--check-current-player))
    (mpris--set-property-sync property value service))
   (t
    (unless mpris--dbus-player-existance-watcher
      (mpris--setup-watchers))
    'no-player)))

;;; State updates

(defun mpris--setup-watchers ()
  "Register signal handlers for the current players."
  (mpris--teardown-watchers)
  (mpris--list-dbus-services
   (mpris--update-service-list
    #'mpris--setup-watchers-1)))

(defun mpris--setup-watchers-1 ()
  "Fulfil the promise of `mpris--setup-watchers'."
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
          mpris--after-init-callbacks)
    (setq mpris--after-init-callbacks nil)))

(defun mpris--teardown-watchers ()
  "Unregister any watchers created by `mpris--setup-watchers'."
  (when mpris--dbus-player-existance-watcher
    (dbus-unregister-object mpris--dbus-player-existance-watcher)
    (setq mpris--dbus-player-existance-watcher nil))
  (mapc #'mpris--remove-player-watcher mpris--player-states))

(defun mpris--create-player-watcher (player-service)
  "Create a watcher for PLAYER-SERVICE and add it to `mpris--player-states'."
  (let ((watcher
         (dbus-register-signal
          :session
          player-service
          mpris--dbus-path
          dbus-interface-properties
          "PropertiesChanged"
          (mpris--create-player-property-change-handler player-service)
          mpris--dbus-interface)))
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
              (run-hook-with-args 'mpris-current-status-hook 'playback value)))
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
  (mpris--get-property
   "PlaybackStatus"
   (lambda (status)
     (mpris--update-state-attr service-name :playback-status (car-safe status)))
   service-name))

(defun mpris--update-metadata (service-name)
  "Update the metadata entry for SERVICE-NAME in `mpris--player-states'."
  (mpris--get-property
   "Metadata"
   (lambda (metadata)
     (mpris--update-state-attr service-name :metadata (car-safe metadata)))
   service-name))

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
    ;; finished initialising all of `mpris--player-states' (likely after adding
    ;; a new player). As such, it makes sense to update the current player.
    (when (and did-initialise-attr (not incomplete))
      (mpris-update-current-player)
      ;; If `mpris--after-init-callbacks' is non-nil, this should be the first
      ;; complete initialisation, as such we should run those callbacks.
      (when mpris--after-init-callbacks
        (mapc (lambda (fn-args) (apply (car fn-args) (cdr fn-args)))
              mpris--after-init-callbacks)
        (setq mpris--after-init-callbacks nil)))))

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

;;; The current player

(defun mpris-update-current-player ()
  "Check and update the player currently being interfaced with.
This relies on active DBus watchers maintaining `mpris--active-players'
and `mpris--player-states'."
  ;; If the current player exists and is playing, do nothing.
  (unless (and (member mpris-current-player mpris--active-players)
               (equal "Playing" (mpris-get-playback-status)))
    (let ((candidate-services mpris--active-players)
          (previous-service mpris-current-player)
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
           ((member name mpris-disliked-players)
            (push service disliked-services))
           ((member service mpris--recent-players)
            (push service recent-services))
           ((member name mpris-preferred-players)
            (push service preferred-services))
           (t (push service common-services)))))
      ;; First, look for playing services. With priority order:
      ;; 1. Preferred
      ;; 2. Recently played
      ;; 3. Remaining (not disliked)
      ;; 4. Disliked services
      (dolist (service-sublist
               (list recent-services preferred-services common-services disliked-services))
        (dolist (service service-sublist)
          (unless selected-service
            (when (equal "Playing" (mpris--get-property-sync "PlaybackStatus" service))
              (setq selected-service service)))))
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
        (setq mpris--recent-players (delete selected-service mpris--recent-players))
        (push selected-service mpris--recent-players)
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
    (push (cons callback-fn callback-args) mpris--after-init-callbacks)
    (mpris--setup-watchers))))

;;; MPRIS API - Methods

(defun mpris-next (&optional async-handler service)
  "Skip to the next track in the tracklist.
If there is no next track (and endless playback and track repeat are both off),
stop playback. If playback is paused or stopped, it remains that way.

If CanGoNext is false, attempting to call this method should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service "Next"))

(defun mpris-previous (&optional async-handler service)
  "Skips to the previous track in the tracklist.
If there is no previous track (and endless playback and track repeat are both
off), stop playback.  If playback is paused or stopped, it remains that way.

If CanGoPrevious is false, attempting to call this method should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service "Previous"))

(defun mpris-pause (&optional async-handler service)
  "Pauses playback.
If playback is already paused, this has no effect. Calling Play after this
should cause playback to start again from the same position.

If CanPause is false, attempting to call this method should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service "Pause"))

(defun mpris-play-pause (&optional async-handler service)
  "Pause or resume playback.
If playback is already paused, resumes playback. If playback is stopped, starts
playback.

If CanPause is false, attempting to call this method should have
no effect and raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service "PlayPause"))

(defun mpris-stop (&optional async-handler service)
  "Stops playback.
If playback is already stopped, this has no effect. Calling Play after this
should cause playback to start again from the beginning of the track.

If CanControl is false, attempting to call this method should have no effect and
raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service "Stop"))

(defun mpris-play (&optional async-handler service)
  "Start or resume playback.

If already playing, this has no effect.
If paused, playback resumes from the current position.
If there is no track to play, this has no effect.

If CanPlay is false, attempting to call this method should have no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (interactive (list #'ignore))
  (mpris--call-method async-handler service "Play"))

(defun mpris-seek (offset &optional async-handler service)
  "Seeks forward in the current track by OFFSET microseconds.
A negative value seeks back. If this would mean seeking back further than the
start of the track, the position is set to 0.  If the value passed in would mean
seeking beyond the end of the track, acts like a call to Next.

If the CanSeek property is false, this has no effect.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--call-method async-handler service "Seek" :int64 offset))

(defun mpris-set-position (track-id position &optional async-handler service)
  "Set the current track (TRACK-ID) playback to POSITION microseconds.

If the Position argument is less than 0, do nothing.
If the Position argument is greater than the track length, do nothing.

If the CanSeek property is false, this has no effect.

The TRACK-ID argument is required to avoid race conditions where
a client tries to seek a position after the track has already
been changed.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called on completion with a single nil argument.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--call-method async-handler service "SetPosition"
                      :object-path track-id :int64 position))

(defun mpris-open-uri (uri &optional async-handler service)
  "Open URI with the current player.

The uri scheme should be an element of the
org.mpris.MediaPlayer2.SupportedUriSchemes property and the
mime-type should match one of the elements of the
org.mpris.MediaPlayer2.SupportedMimeTypes.

If the uri scheme or the mime-type of the uri to open is not
supported, this method does nothing and may raise an error. In
particular, if the list of available uri schemes is empty, this
method may not be implemented.

Clients should not assume that the Uri has been opened as soon as
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
  (mpris--call-method async-handler service "OpenUri" uri))

;;; MPRIS API - Properties

(defun mpris-get-playback-status (&optional async-handler service)
  "Get the current playback status.

Either \"Playing\", \"Paused\", or \"Stopped\".

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if-let ((value
            (and mpris--player-states
                 (plist-get
                  (cdr (assoc (or service mpris-current-player)
                              mpris--player-states))
                  :playback-status))))
      (if async-handler
          (funcall async-handler value)
        value)
    (mpris--get-property "PlaybackStatus" async-handler service)))

(defun mpris-get-loop-status (&optional async-handler service)
  "Get the current loop/repeat status.

May be:
- \"None\" if the playback will stop when there are no more tracks to play.
- \"Track\" if the current track will start again from the begining once it has
  finished playing.
- \"Playlist\" if the playback loops through a list of tracks.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "LoopStatus" async-handler service))

(defun mpris-set-loop-status (value &optional async-handler service)
  "Set the current loop/repeat status to VALUE.

VALUE may be:
- \"None\" the playback will stop when there are no more tracks to play.
- \"Track\" the current track will start again from the begining once it has
  finished playing.
- \"Playlist\" the playback loops through a list of tracks.

If CanControl is false, attempting to set this property should
have no effect and raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--set-property "LoopStatus" value async-handler service))

(defun mpris-get-rate (&optional async-handler service)
  "Get the current playback rate.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "Rate" async-handler service))

(defun mpris-set-rate (value &optional async-handler service)
  "Set the current playback rate to VALUE.

The value must fall in the range described by MinimumRate and
MaximumRate, and must not be 0.0. If playback is paused, the
PlaybackStatus property should be used to indicate this. A value
of 0.0 should not be set by the client. If it is, the media
player should act as though Pause was called.

If the media player has no ability to play at speeds other than
the normal playback rate, this must still be implemented, and
must return 1.0. The MinimumRate and MaximumRate properties must
also be set to 1.0.

Not all values may be accepted by the media player. It is left to
media player implementations to decide how to deal with values
they cannot use; they may either ignore them or pick a best fit
value. Clients are recommended to only use sensible fractions or
multiples of 1 (eg: 0.5, 0.25, 1.5, 2.0, etc).

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--set-property "Rate" value async-handler service))

(defun mpris-get-shuffle (&optional async-handler service)
  "Whether the playlist is progressing in a non-linear order.

This property is optional. Clients should handle its absence gracefully.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "Shuffle" async-handler service))

(defun mpris-set-shuffle (value &optional async-handler service)
  "Set the playlist to progress in a non-linear order depending on VALUE.

VALUE should be either t or nil.

This property is optional. Clients should handle its absence gracefully.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--set-property "Shuffle" value async-handler service))

(defun mpris-get-metadata (&optional async-handler service)
  "Get the metadata of the current track.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (if-let ((value
            (and mpris--player-states
                 (plist-get
                  (cdr (assoc (or service mpris-current-player)
                              mpris--player-states))
                  :metadata))))
      (if async-handler
          (funcall async-handler value)
        value)
    (mpris--get-property "Metadata" async-handler service)))

(defun mpris-get-volume (&optional async-handler service)
  "Get the volume level.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "Volume" async-handler service))

(defun mpris-set-volume (value &optional async-handler service)
  "Set the volume level to VALUE.

When setting, if a negative value is passed, the volume should be
set to 0.0.

If CanControl is false, attempting to set this property should
have no effect and raise an error.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--set-property "Volume" value async-handler service))

(defun mpris-get-position (&optional async-handler service)
  "The current track position in microseconds.

The position is between 0 and the \"mpris:length\" metadata
entry (see Metadata).

Note: If the media player allows it, the current playback
position can be changed either the SetPosition method or the Seek
method on this interface. If this is not the case, the CanSeek
property is false, and setting this property has no effect and
can raise an error.

If the playback progresses in a way that is inconstistant with
the Rate property, the Seeked signal is emited.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "Position" async-handler service))

(defun mpris-get-minimum-rate (&optional async-handler service)
  "The minimum value which the Rate property can take.

Clients should not attempt to set the Rate property below this value.

Note that even if this value is 0.0 or negative, clients should
not attempt to set the Rate property to 0.0.

This value should always be 1.0 or less.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "MinimumRate" async-handler service))

(defun mpris-get-maximum-rate (&optional async-handler service)
  "The maximum value which the Rate property can take.

Clients should not attempt to set the Rate property above this value.

This value should always be 1.0 or greater.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "MaximumRate" async-handler service))

(defun mpris-can-go-next (&optional async-handler service)
  "Whether `mpris-next' is expected to change the current track.

More specifically, whether the client can call the Next method on
this interface and expect the current track to change.

If it is unknown whether a call to Next will be successful (for
example, when streaming tracks), this property should be set to
true.

If CanControl is false, this property should also be false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "CanGoNext" async-handler service))

(defun mpris-can-go-previous (&optional async-handler service)
  "Whether `mpris-previous' is expected to change the current track.

More specifically, whether the client can call the Previous
method on this interface and expect the current track to change.

If it is unknown whether a call to Previous will be
successful (for example, when streaming tracks), this property
should be set to true.

If CanControl is false, this property should also be false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "CanGoPrevious" async-handler service))

(defun mpris-can-play (&optional async-handler service)
  "Whether playback can be started using Play or PlayPause.

Note that this is related to whether there is a \"current
track\": the value should not depend on whether the track is
currently paused or playing. In fact, if a track is currently
playing (and CanControl is true), this should be true.

If CanControl is false, this property should also be false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "CanPlay" async-handler service))

(defun mpris-can-pause (&optional async-handler service)
  "Whether playback can be paused using Pause or PlayPause.

Note that this is an intrinsic property of the current track: its
value should not depend on whether the track is currently paused
or playing. In fact, if playback is currently paused (and
CanControl is true), this should be true.

If CanControl is false, this property should also be false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "CanPause" async-handler service))

(defun mpris-can-seek (&optional async-handler service)
  "Whether the client can control the playback position using Seek and SetPosition.

This may be different for different tracks.

If CanControl is false, this property should also be false.

When ASYNC-HANDLER is set, the call is made asynchronously and the function
called with the result.

To use a specific player, set SERVICE to the target MediaPlayer2 service.

If SERVICE is unset and no player exists, the symbol no-player is returned."
  (mpris--get-property "CanSeek" async-handler service))

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
  (mpris--get-property "CanControl" async-handler service))

;;; Utility functions

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
                   (url-filename
                    (url-generic-parse-url (car v))))))
           (_ #'car)))
        (metadata (mpris-get-metadata nil service)))
    (and (not (eq metadata 'no-player))
         (funcall value-extractor (cadr (assoc key metadata))))))

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

(provide 'mpris)
;;; mpris.el ends here
