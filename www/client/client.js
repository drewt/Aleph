/*
 * Copyright (c) 2016 Drew Thoreson
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
 * IN THE SOFTWARE.
 */

/*
 * HTML generation API.
 */
var AlephHTML = (function() {
  return {
    formatFeed: function(feed) {
      var div = document.createElement("div");
      div.setAttribute("class", "feed");
      div.innerHTML =
        '<h1 class="feed-title"><a href="' + feed.link + '">' + feed.title + '</a></h1>' +
        '<a class="feed-source" href="' + feed.source + '">Source</a>' +
        '<span class="feed-description">' + feed.description + '</span>';
      return div;
    },
    formatItem: function(item) {
      var div = document.createElement("div");
      div.setAttribute("class", "item");
      div.innerHTML =
        '<h2 class="item-title"><a href="' + item.link + '">' + item.title + '</a></h2>' +
        '<span class="item-published">' + new Date(item.published*1000).toUTCString() + '</span>' +
        '<span class="item-content">' + item.content + '</span>';
      return div;
    },
  }
})();

/*
 * Get the value of the parameter @name from the query string of the page URI.
 */
function queryParameter(name) {
  var result = null;
  var tmp = [];
  location.search.substr(1).split("&").forEach(function(item) {
    tmp = item.split("=");
    if (tmp[0] === name)
      result = decodeURIComponent(tmp[1]);
  });
  return result;
}
