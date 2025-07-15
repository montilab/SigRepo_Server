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

function toggle_change_password(){
  const passwordField = document.getElementById('new_profile_password');
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

function sig_tbl_select_all(){
  var all = document.getElementById('sig_tbl_select_all');
  var checkboxes = document.getElementsByName('sig_tbl_select_row');
  if(all[0].checked === true){
    for(var i = 0; i < checkboxes.length; i++){
      checkboxes[i].checked = true;
    }
  }else{
    for(var i = 0; i < checkboxes.length; i++){
      checkboxes[i].checked = false;
    }
  }
}

function sig_tbl_select_rows() {
  var checkboxes = document.getElementsByName('sig_tbl_select_row');
  var row_number = [];
  for(var i = 0; i < checkboxes.length; i++){
    if(checkboxes[i].checked === true){
      row_number.push(checkboxes[i].value);
    }else{
      row_number.push(null);
    }
  }
 Shiny.onInputChange('sig_tbl_selected_rows', row_number);
}


function get_up_regulated_id(id) {
  //alert("here");
  var id = String(id);
  Shiny.onInputChange('sig_up_regulated_id', id);
}
  
function get_down_regulated_id(id) {
  //alert("there");
  var id = String(id);
  Shiny.onInputChange('sig_down_regulated_id', id);
}  

