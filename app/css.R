
#This is for loading message and spinner
mycss <- "
#plot-container1 {
  position: absolute; left: 50%; top: 18%; z-index: -1;
}
#plot-container2 {
  position: absolute; left: 50%; top: 58%; z-index: -1;
}
#plot-container5 {
  position: absolute; left: 50%; top: 25%; z-index: -1;
}
#plot-container6 {
  position: absolute; left: 50%; top: 70%; z-index: -1;
}
#loading-spinner1 {
  position: absolute; left: 50%; top: 50%; z-index: -1;
  margin-top: -1px;
  margin-left: -80px;
}
#loading-spinner2 {
  position: absolute; left: 50%; top: 50%; z-index: -1;
  margin-top: -1px;  /* half of the spinner's height */
  margin-left: -80px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
#loadmessage1 {
  position: absolute; top: 23%; left: 10%; width: 80%; padding: 5px 0px 5px 0px; 
  text-align: center; font-size:130%;  font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage2 {
  position: absolute; top: 50%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage5 {
  position: absolute; top: 30%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
#loadmessage6 {
  position: absolute; top: 90%; left: 10%; width: 80%; padding: 5px 0px 5px 0px;
  text-align: center; font-size:130%; font-style:italic; color: #708090;
  background-color:white; z-index: -1;
}
.proposed {
  font-style: italic;
  font-weight: bold;
  color: red;
}
"