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

function toggle_register_password(){
  const passwordField = document.getElementById('register_password');
  const passwordFieldType = passwordField.getAttribute('type');
  if(passwordFieldType === 'password'){
    passwordField.setAttribute('type', 'text');
  }else{
    passwordField.setAttribute('type', 'password');
  }
}

var tabs = ["home", "signatures", "collections", "compare", "annotate", "resources"]

function select_navtab(tab){
  var id = String(tab);
  for (var i = 0; i < tabs.length; i++) {
    if(tabs[i] === id){
      document.getElementById(tabs[i]).classList.add("active");
      document.getElementById(tabs[i]+"-"+"container").classList.remove("invisible");
    }else{
      document.getElementById(tabs[i]).classList.remove("active");
      document.getElementById(tabs[i]+"-"+"container").classList.add("invisible");
    }
  }
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

$(document).on("click", ".edit-btn", function() {
  var id = $(this).data("id");
  Shiny.setInputValue("signature_id_to_edit", id, { priority: "event" });
});
$(document).on("click", ".delete-btn", function() {
  var id = $(this).data("id");
  Shiny.setInputValue("signature_id_to_delete", id, { priority: "event" });
});


