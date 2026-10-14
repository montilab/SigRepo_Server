/*
  Enter submits the sign-in form, and the eye icons beside the password
  fields (sign-in, register, change password) show or hide what was typed.
*/

function login_keypress(e){
  if(e.which === 13){
    e.preventDefault();
    // Shiny's text bindings send keystrokes after a short debounce, so the
    // server could still be holding the last characters typed when the click
    // arrives. Send both fields first; they and the click travel in order.
    Shiny.setInputValue("username", document.getElementById("username").value);
    Shiny.setInputValue("password", document.getElementById("password").value);
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
