angular.
  module('alephApp').
  config(['$locationProvider', '$routeProvider',
      function config($locationProvider, $routeProvider) {
        $locationProvider.hashPrefix('!');

        var itemListRoute = {
          template: '<item-list></item-list>'
        };
        var editFeedRoute = {
          template: '<p>EDIT</p>'
        };
        var addFeedRoute = {
          template: '<add-feed></add-feed>'
        };
        $routeProvider.
          when('/feeds/', itemListRoute).
          when('/feeds/:feedId/', itemListRoute).
          when('/feeds/:feedId/edit/', editFeedRoute).
          when('/feeds/:feedId/:modifier/', itemListRoute).
          when('/add-feed/', addFeedRoute).
          otherwise('feeds');
      }
  ]);
