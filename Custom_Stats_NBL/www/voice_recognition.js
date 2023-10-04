document.addEventListener('DOMContentLoaded', (event) => {
  var recognition = new webkitSpeechRecognition();
  recognition.continuous = true;
  recognition.lang = 'en-US';  // You can set this to 'en-AU' for Australian English if preferred
  recognition.interimResults = false;
  recognition.maxAlternatives = 1;

  // Automatically start recognition
  recognition.start();

  recognition.onresult = function(event) {
      var last = event.results.length - 1;
      var command = event.results[last][0].transcript.trim().toLowerCase();

      Shiny.setInputValue("voice_command", command, {priority: 'event'});
      recognition.stop();  // Stop after getting a command, to be restarted later
  };

  recognition.onend = function() {
      recognition.start();  // Restart the recognition once it ends
  };
});
