function doNotifySend(message) {
  var n = new Notification(message);
  setTimeout(n.close.bind(n), 5000);
}

function notifySend(message) {
  // if the browser doesn't support notification, log the message instead
  if (!("Notification" in window)) {
    console.log('Notification: ' + message);
  }
  // if permission was already granted, send the notification
  else if (Notification.permission === 'granted') {
    doNotifySend(message);
  }
  // ask for permission if it hasn't been denied
  else if (Notification.permission !== 'denied') {
    Notification.requestPermission(function(permission) {
      if (permission === 'granted')
        doNotifySend(message);
      else
        console.log('Notification: ' + message);
    });
  }
}

angular.
  module('feedList').
  component('feedList', {
    templateUrl: 'app/feed-list/feed-list.template.html',
    controller: function FeedListController($scope, $http, $interval) {
      var self = this;

      this.mergeFeeds = function(feeds) {
        // FIXME: there HAS to be a better way of doing this
        for (var newi = 0; newi < feeds.length; newi++) {
          var newFeed = feeds[newi];
          for (var oldi = 0; oldi < self.feeds.length; oldi++) {
            var oldFeed = self.feeds[oldi];
            if (oldFeed.id == newFeed.id) {
              if (oldFeed.unread < newFeed.unread) {
                notifySend(feeds[newi].name + ' updated (' + feeds[newi].unread + ' unread)');
              }
              if (oldFeed.unread != newFeed.unread) {
                self.unread = Math.max(self.unread + (newFeed.unread - oldFeed.unread), 0);
              }
              self.feeds[oldi] = newFeed;
            }
          }
        }
      };

      this.update = function(id) {
        $http.post('/feeds/' + id + '/update').then(function(response) {
          self.mergeFeeds([response.data]);
        });
      };
      this.markRead = function(id) {
        $http.post('/feeds/' + id + '/mark-read');
        for (i = 0; i < self.feeds.length; i++) {
          if (self.feeds[i].id == id && self.feeds[i].unread > 0) {
            self.unread = Math.max(self.unread - self.feeds[i].unread, 0);
            self.feeds[i].unread = 0;
          }
        }
      };
      this.deleteFeed = function(id) {
        $http.delete('/feeds/' + id);
      };
      this.refresh = function() {
        $http.get('/feeds').then(function(response) {
          self.feeds = response.data;
          var count = 0;
          for (i = 0; i < self.feeds.length; i++) {
            count += self.feeds[i].unread;
          }
          self.unread = count;
        });
      };

      // Get initial feed list.
      this.refresh();

      // Schdule update every 60 seconds
      this.timer = $interval(function refreshFeeds() {
        // XXX: 65 = 60 seconds inverval + 5 seconds headroom for RTT
        $http.get('/feeds?since=-65').then(function(response) {
          self.mergeFeeds(response.data);
        });
      }, 60000)

      $scope.$on("$destroy", function(event) {
        $interval.cancel(self.timer);
      });
      $scope.$on('refresh-feeds', function(event) {
        self.refresh();
      });
      $scope.$on('mark-all-read', function(event) {
        for (i = 0; i < self.feeds.length; i++) {
          self.feeds[i].unread = 0;
        }
        self.unread = 0;
      });
    }
  });
