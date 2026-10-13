/*
  Enter submits the sign-in form, and the eye icons beside the password
  fields (sign-in, register, change password) show or hide what was typed.
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
