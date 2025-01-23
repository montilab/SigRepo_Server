/*
	javascript for the app
*/

function login_keypress(e){
  if(e.which === 13){
    document.getElementById("sign_in_btn").click();
  }
}

function toggle_password(){
  const passwordField = document.getElementById('password');
  const passwordFieldType = passwordField.getAttribute('type');
  if(passwordFieldType === 'password'){
    passwordField.setAttribute('type', 'text');
  }else{
    passwordField.setAttribute('type', 'password');
  }
}

function select_navtab(tab){
  var tab = String(tab);
  Shiny.onInputChange("selected_tab", tab);
}