angular.
  module('itemList').
  component('itemList', {
    templateUrl: 'app/item-list/item-list.template.html',
    controller: ['$routeParams', '$http',
      function ItemListController($routeParams, $http) {
        var self = this;
        var url = '/items';
        if ($routeParams.feedId === 'unread') {
          url += '?unread=true';
        }
        else if ($routeParams.feedId) {
          url = '/feeds/' + $routeParams.feedId + '/items';
          if ($routeParams.modifier) {
            switch ($routeParams.modifier) {
              case "unread":
                url += '?unread=true';
                break;
            }
          }
        }
        $http.get(url).then(function(response) {
          self.items = response.data;
          for (i = 0; i < self.items.length; i++) {
            self.items[i].published = new Date(self.items[i].published*1000).toUTCString();
          }
        });
      }
    ]
  });
