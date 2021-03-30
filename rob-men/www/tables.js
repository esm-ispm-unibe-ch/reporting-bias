document.body.style.backgroundColor = 'skyblue';
console.log('hello');

function table1select(id){
  var x = document.getElementById(id).value;

	console.log("you selected",id);
	console.log("value",x);
	var out = {id:id, value:x};

	Shiny.setInputValue("table1select", out);
}

function startAnalysis () {
  var r = confirm("Start Analysis?");
  if (r === true) {
    console.log("starting analysis")
  	Shiny.setInputValue("startAnalysis", true);
  } else {
    console.log("canceled")
  }
}

function resetAnalysis () {
  var r = confirm("Reset Analysis?; Everything will be lost");
  if (r === true) {
    console.log("reset")
  	Shiny.setInputValue("resetAnalysis", true);
  } else {
    console.log("canceled")
  }
}

function table2col4select(id){
  var x = document.getElementById(id).value;

	console.log("you selected",id);
	console.log("value",x);
	var out = {id:id, value:x};

	Shiny.setInputValue("table2col4", out);
}

function table2col8select(id){
  var x = document.getElementById(id).value;

	console.log("you selected",id);
	console.log("value",x);
	var out = {id:id, value:x};

	Shiny.setInputValue("table2col8", out);
}

function table2finalselect(id){
  var x = document.getElementById(id).value;

	console.log("you selected",id);
	console.log("value",x);
	var out = {id:id, value:x};

	Shiny.setInputValue("table2final", out);
}