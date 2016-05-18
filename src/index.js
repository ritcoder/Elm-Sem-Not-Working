'use strict';

require("./index.html");

require("semantic.css");
require("./Styles/app");

require("./app/assets.js");

//add notification stuff
require("toastr.css");
var toastr = require("toastr");
toastr.options =
  { preventDuplicates: true, progressBar: true, closeButton: true };

var Elm = require("no-export!./Main"); 

//remove the intial html
var loadingElmement = document.getElementById('main');
document.body.removeChild(loadingElmement);

//setup the initial application settings
var startInfo =
  { serverUrl :
      location.hostname == "localhost" 
        ? "http://localhost:61009" 
        : (location.protocol + "//" + location.host)
  };

var app = Elm.Main.fullscreen(startInfo);

app.ports.toastr.subscribe(function(msg){
  toastr[msg.op](msg.message, msg.title);
});

app.ports.logout.subscribe(function(){
  app.ports.clearUser.send(0);
});

app.ports.openWindow.subscribe(function(config){
  console.log("in new window "+ JSON.stringify(config))
  app.ports.createWindow.send(config);
});

window.toastr = toastr;
window.app = app;