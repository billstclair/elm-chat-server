//////////////////////////////////////////////////////////////////////
//
// elmNotifications.js
// JavaScript code for billstclair/elm-system-notifications
// Copyright (c) 2018 Bill St. Clair <billstclair@gmail.com>
// Some rights reserved.
// Distributed under the MIT License
// See LICENSE.txt
//
//////////////////////////////////////////////////////////////////////

var elmNotifications = {};

(function() {

  elmNotifications.isAvailable = isAvailable;
  elmNotifications.requestPermission = requestPermission;
  elmNotifications.notify = notify;

  function isAvailable () {
    return typeof(Notification) == "function";
  };

  function requestPermission(callback) {
    var permission = Notification.permission;
    if (permission != "default") {
      callback(permission);
    } else {
      Notification.requestPermission(callback)
    }
  }

  function notify(title, body, img) {
    if (isAvailable()) {
      requestPermission(function(permission) {
        if (permission == "granted") {
          var options = {};
          if (body) { options.body = body };
          if (img) { options.icon = img };
          var notification = new Notification(title, options);
          setTimeout(function() { notification.close.bind(notification)() }, 4000);
        }
      });
    }
  }
})();
