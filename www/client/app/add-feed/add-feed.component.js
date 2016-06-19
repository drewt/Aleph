angular.
  module('addFeed').
  component('addFeed', {
    templateUrl: 'app/add-feed/add-feed.template.html',
    controller: function AddFeedController($http) {
      var self = this;

      this.name = '';
      this.source = '';
      this.fetcher = 'http';
      this.parser = 'auto';
      this.interval = 30;

      this.submitFeed = function() {
        $http({
          method: 'POST',
          url: '/add-feed', 
          headers: {'Content-Type': 'application/x-www-form-urlencoded'},
          transformRequest: function(obj) {
            var str = [];
            for (var p in obj)
              str.push(encodeURIComponent(p) + '=' + encodeURIComponent(obj[p]));
            return str.join('&');
          },
          data: {
            'name': self.name,
            'source': self.source,
            'fetcher': self.fetcher,
            'parser': self.parser,
            'scheduleparameter': self.interval
          }
        });
      };
    }
  });
