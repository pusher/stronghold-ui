(function() {
  function isObject(x) {
    return x && typeof x == 'object' && x.constructor == Object;
  }

  function deepMerge(a, b) {
    if (isObject(a) && isObject(b)) {
      var result = {};
      for (key in a) {
        result[key] = a[key];
      }
      for (key in b) {
        if (key in a) {
          result[key] = deepMerge(a[key], b[key]);
        } else {
          result[key] = b[key];
        }
      }
      return result;
    } else {
      return b;
    }
  }

  $(document).ready(function() {
    var peculiar_container = $('#peculiar-editor');
    var materialized_container = $('#materialized-view');

    var peculiar_json = peculiar_container.data('json');
    var materialized_json = materialized_container.data('json');

    var peculiar_editor =
      new jsoneditor.JSONEditor(
        peculiar_container[0],
        {},
        peculiar_json
      );
    var materialized_view =
      new jsoneditor.JSONEditor(
        materialized_container[0],
        {'mode': 'viewer', 'history':false}
      );

    $('#mode>button').each(function(_, element) {
      $(element).click(function() {
        if (element.value == '1') {
          // switch to materialized
          var json = deepMerge(materialized_json, peculiar_editor.get());
          materialized_view.set(json);
          $('#peculiar-editor').css('display', 'none');
          $('#materialized-view').css('display', 'block');
        } else {
          // switch to peculiar
          $('#peculiar-editor').css('display', 'block');
          $('#materialized-view').css('display', 'none');
        }
      });
    });

    $('#update-form').submit(function() {
      $('#json-field').val(JSON.stringify(peculiar_editor.get()));
    })
  });
})();
