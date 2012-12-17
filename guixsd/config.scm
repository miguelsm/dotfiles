; vim :set ft=lisp et
;; This is an operating system configuration template
;; for a "bare bones" setup, with no X11 display server.

(use-modules (gnu) (srfi srfi-1))
(use-service-modules desktop xorg)
(use-package-modules certs emacs screen shells ssh xorg)

(operating-system
  (host-name "cubi2")
  (timezone "Europe/London")
  (locale "en_US.utf8")
  (hosts-file (local-file (string-append (getenv "HOME")
                                         "/hosts")))

  ;; Assuming /dev/sdX is the target hard disk, and "my-root" is
  ;; the label of the target root file system.
  (bootloader (bootloader-configuration
                (bootloader grub-bootloader)
                (target "/dev/sda")))
  (file-systems (cons* (file-system
                         (device "root")
                         (title 'label)
                         (mount-point "/")
                         (type "ext4"))
                       (file-system
                         (device "home")
                         (title 'label)
                         (mount-point "/home")
                         (type "ext4"))
                       %base-file-systems))

  ;; This is where user accounts are specified.  The "root"
  ;; account is implicit, and is initially created with the
  ;; empty password.
  (users (cons (user-account
                 (name "miguelsm")
                 (comment "")
                 (group "users")

                 ;; Adding the account to the "wheel" group
                 ;; makes it a sudoer.  Adding it to "audio"
                 ;; and "video" allows the user to play sound
                 ;; and access the webcam.
                 (supplementary-groups '("wheel"
                                         "audio"
                                         "lp"
                                         "video"
                                         "docker"))
                 (home-directory "/home/miguelsm"))
               %base-user-accounts))

  (groups (cons (user-group (name "docker")) %base-groups))

  ;; Globally-installed packages.
  (packages (cons* emacs-exwm xinit xorg-server screen openssh nss-certs %base-packages))

  (services (cons (bluetooth-service)
                  %desktop-services)))
