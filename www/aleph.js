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
 * Aleph JS API.
 */
var Aleph = (function() {

  /*
   * Do an HTTP GET request for @url, calling @cb with the result as text and
   * the HTTP status code.
   */
  function AJAXGet(url, cb) {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (xhttp.readyState == 4)
        cb(xhttp.responseText, xhttp.status);
    };
    xhttp.open("GET", url, !!cb);
    xhttp.send();
  }

  function AJAXPostForm(url, form, cb) {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
      if (xhttp.readyState == 4)
        cb(xhttp.responseText, xhttp.status);
    };
    xhttp.open("POST", url, !!cb);
    xhttp.send(form);
  }

  function AJAXPost(url, params, cb) {
    // put the params into a FormData object
    var form = new FormData();
    for (var name in params) {
      form.append(name, params[name]);
    }
    AJAXPostForm(url, form, cb);
  }

  return {
    getFeeds: function(cb) {
      AJAXGet("/feeds", function(text, code) {
        cb(code >= 400 ? [] : JSON.parse(text), code)
      });
    },
    getFeed: function(id, cb) {
      AJAXGet("/feeds/" + id, function(text, code) {
        cb(code >= 400 ? null : JSON.parse(text), code);
      });
    },
    getItems: function(id, cb) {
      AJAXGet("/feeds/" + id + "/items", function(text, code) {
        cb(code >= 400 ? [] : JSON.parse(text), code);
      });
    },
    updateFeed: function(id, cb) {
      AJAXPost("/feeds/" + id + "/update", [], function(text, code) {
        cb(code);
      });
    },
    markFeedRead: function(id) {
      AJAXPost("/feeds/" + id + "/mark-read", [], function(text, code) {});
    },
    addFeed: function(form, cb) {
      AJAXPostForm("/add-feed", form, function(text, code) {
        cb(code >= 400 ? null : JSON.parse(text), code);
      });
    },
  }
})();
