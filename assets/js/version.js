(function() {
  $(document).ready(function() {
    var form = $('#navigate-hierarchy').submit(function() {
      var path = form[0].children[0].value;
      var version = document.location.pathname.split('/')[1];
      document.location.pathname = '/' + version + '/node' + path;
      return false;
    });
  });
})();
