angular.
  module('toolBar').
  component('toolBar', {
    templateUrl: 'app/tool-bar/tool-bar.template.html',
    controller: function ToolBarController($http, $rootScope) {
      var self = this;
      this.refresh = function() {
        $rootScope.$broadcast('refresh-feeds');
      };
      this.markAllRead = function() {
        $http.post('/feeds/mark-read');
        $rootScope.$broadcast('mark-all-read');
      };
      this.updateAll = function() {
        $http.post('/feeds/update');
      };
    }
  });

