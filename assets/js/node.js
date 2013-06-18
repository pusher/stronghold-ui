(function() {
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
        {'mode': 'viewer', 'history':false},
        materialized_json
      );

    $('#mode>button').each(function(_, element) {
      $(element).click(function() {
        // TODO: update the materialized view with data from the peculiar view
        if (element.value == '1') {
          $('#peculiar-editor').css('display', 'none');
          $('#materialized-view').css('display', 'block');
        } else {
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
