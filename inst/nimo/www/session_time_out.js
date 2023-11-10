function idleTimer() {
  var t;

  // Function to get the timeout value from the input element
  function getTimeoutValue() {
    return parseInt($("#time_out").val()); // Read the value from the input element
  }

  function logout() {
    Shiny.setInputValue('timeOut', getTimeoutValue());
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, getTimeoutValue());
  }

  // Initial setup
  t = setTimeout(logout, getTimeoutValue());
  window.onmousemove = resetTimer;
  window.onmousedown = resetTimer;
  window.onclick = resetTimer;
  window.onscroll = resetTimer;
  window.onkeypress = resetTimer;
}

idleTimer();

