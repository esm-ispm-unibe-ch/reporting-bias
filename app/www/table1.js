document.body.style.backgroundColor = 'skyblue';
console.log('hello');

function table1select(id){
  var x = document.getElementById(id).value;

	console.log("you selected",id);
	console.log("value",x);
	var out = {id:id, value:x};

	Shiny.setInputValue("table1select", out);
}
