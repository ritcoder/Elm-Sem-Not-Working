/* global xlsx */
/* global Elm */
Elm.Native = Elm.Native || {};
Elm.Native.Xlsx = {};


Elm.Native.Xlsx.make = function(localRuntime){
  localRuntime.Native = localRuntime.Native || {};
  localRuntime.Native.Xlsx = localRuntime.Native.Xlsx || {};
  
  if (localRuntime.Native.Xlsx.values)
      return localRuntime.Native.Xlsx.values;
  
  var Task = Elm.Native.Task.make(localRuntime);
  
  // readFromBinaryString : String -> Task error String
  var readFromBinaryString = function(filedata){
    //todo: get the work book, etc
    return Task.asyncFunction(function(callback){
      try{
        var byteString = atob(filedata);
        var workbook = xlsx.read(byteString, { type: "binary"})
        // var sheets = workbook.SheetNames;
        var output = {};
        for(var name in workbook.Sheets){
          output[name] = xlsx.utils.sheet_to_json(workbook.Sheets[name]);
        }
        var result = { sheetNames: workbook.SheetNames, sheets: output };
        console.log("result", result);
        return callback(Task.succeed(result));
      }
      catch(e){
        return callback(Task.fail({ctor: "Error", _0: e && e.message || e}))
      }
      // debugger;
      //todo: try-catch
      //todo: create the workbook
      //todo: see if you can pass an object with temps
      
      // return callback(Task.succeed("This is from read native ::"+filedata))
    });
  }
  
  return {
    readFromDataUrl : readFromBinaryString
  };
}