$('.btn').tooltip({
  trigger: 'click',
  placement: 'bottom'
});

function setTooltip(btn, message) {
  $(btn).tooltip('hide')
    .attr('data-original-title', message)
    .tooltip('show');
}

function hideTooltip(btn) {
  setTimeout(function() {
    $(btn).tooltip('hide');
  }, 1000);
}

function disposeTooltip(btn) {
  setTimeout(function() {
    $(btn).tooltip('destroy');
  }, 1000);
}

var clipboard = new ClipboardJS('.btn');

clipboard.on('success', function(e) {
  setTooltip(e.trigger, 'Copied !');
  hideTooltip(e.trigger);
  disposeTooltip(e.trigger);
});

clipboard.on('error', function(e) {
  setTooltip(e.trigger, 'Failed !');
  hideTooltip(e.trigger);
  disposeTooltip(e.trigger);
});
