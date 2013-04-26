/**
 * dcg-wk4-sr-kf-011-forms.js (file 03 of 05)
 * Week 4 Assignment
 * David C. Gibbons
 * WEB/420 - Web Programming II
 * Vladimir Gubanov
 * November 13, 2006
 */

/**
 * Function to set a cookie in the user's browser that lets us
 * know if they've already submitted the form.
 */
function setSubmittedCookie()
{
  // let the cookie expire after a week, just in case they have
  // another contact to submit
  var expirationDate = new Date();
  expirationDate.setDate(expirationDate.getDate() + 7);

  document.cookie = encodeURI("contact.info.submitted=true") + "; expires=" + expirationDate.toUTCString();
  return true;
}

/**
 * Function to show the user submitted form data in a
 * nice, organized tabular format.
 */
function showSubmittedData()
{
  if (location.search == "")
  {
    return true;
  }

  // extract the data from the query string format
  var queryData = location.search.substring(1);
  var queryData = queryData.split("&");

  // create a array indexed by field name so we can pull out
  // what form variables we want
  // NOTE - we'd probably really use the JavaScript HashMap
  // object for this in a real implementation
  var formData = new Array();
  for (var i = 0; i < queryData.length; i++)
  {
    var varData = queryData[i].split("=");
    formData[mydecode(varData[0])] = mydecode(varData[1]);
  }

  // dynamically add the user-entered data to our table so
  // they can easily review what they input and the server
  // received
  var table = document.getElementById("dataTable");
  addToTable(table, "Name:", formData["lastname"] + ", " + formData["givenname"]);
  if (formData["address1"] != "")
  {
    addToTable(table, "Mailing Address:", formData["address1"]);
    addToTable(table, "", formData["address2"]);
    addToTable(table, "", formData["address3"]);
    addToTable(table, "City, State, ZIP Code:" , 
               formData["city"] + ", " + formData["state"] + " " + formData["zipcode"] + "-" + formData["zipplus4"]);
  }
  if (formData["emailaddress"] != "")
  {
    addToTable(table, "E-mail Address:", formData["emailaddress"]);
  }
  if (formData["phonenumber"] != "")
  {
    addToTable(table, "Phone Number:", formData["phonenumber"]);
  }

  return true;
}

/**
 * Decode query string data by replacing all plus signs with spaces and
 * then using the decodeURIComponent function to replace all numeric
 * escape values with their real values.
 */
function mydecode(str)
{
  // first, replace all plus signs with spaces
  var mystr = str.replace(/\+/g, " ");

  // next, use decodeURIComponent to replace all of the remaining
  // encoded characers with their actual values
  mystr = decodeURIComponent(mystr);

  return mystr;
}


/**
 * Dynamically adds a data row to the specified table.
 */
function addToTable(table, field, value)
{
  var col = document.createElement("td");
  col.appendChild(document.createTextNode(field));

  var col2 = document.createElement("td");
  col2.appendChild(document.createTextNode(value));

  var row = document.createElement("tr");
  row.appendChild(col);
  row.appendChild(col2);

  // use tbody explicitly to work properly with IE (why?)
  var tableBody = table.getElementsByTagName("tbody").item(0);
  tableBody.appendChild(row);
}


/**
 * Validates that the specified customer form contains valid
 * input.
 */
function validateCustomerForm(customerForm) 
{
  var isValid = true;

  // validate that they provided their full name
  if (customerForm.lastname.value == "" || customerForm.givenname.value == "") 
  {
    isValid = false;
    window.alert("You must provide your last and given name.");
  } 
  else
  {
    // validate that they have entered either a mailing or email address
    var mailingAddressProvided = isMailingAddressProvided(customerForm);
    var emailAddressProvided = isEmailAddressProvided(customerForm);
    if (!mailingAddressProvided && !emailAddressProvided) 
    {
      isValid = false;
      window.alert("You must provide either your mailing address or your e-mail address.");
    }
    else if (mailingAddressProvided)
    {
      isValid = validateMailingAddress(customerForm);
    }
    else if (emailAddressProvided)
    {
      isValid = validateEmailAddress(customerForm);
    }

    // don't worry about the phone number - the formats vary too much to do any real
    // validation, and it isn't required to boot
  }

  return isValid;
}

/**
 * Validates that some enough data has been entered to fulfill
 * a mailing address.
 */
function isMailingAddressProvided(customerForm)
{
  var provided = true;

  if (customerForm.address1.value == "")
  {
    provided = false;
  }
  else if (customerForm.city.value == "")
  {
    provided = false;
  }
  else if (customerForm.state.value == "")
  {
    provided = false;
  }
  else if (customerForm.zipcode.value == "")
  {
    provided = false;
  }

  return provided;
}

/**
 * Validate that enough data has been entered to fulfill
 * an email address.
 */
function isEmailAddressProvided(customerForm)
{
  var provided = true;

  if (customerForm.emailaddress.value == "")
  {
    provided = false;
  }

  return provided;
}

/**
 * Validate that the mailing address is well formed.
 */
function validateMailingAddress(customerForm)
{
  var isValid = true;

  // not much to do here other than validate than the 
  // ZIP code is numeric (only true for USPS addresses...)
  if (isNaN(customerForm.zipcode.value) || isNaN(customerForm.zipplus4.value))
  {
    isValid = false;
    window.alert("ZIP Code must be numeric only");
  }
  else if (customerForm.zipcode.value.length != 5)
  {
    isValid = false;
    window.alert("ZIP code must be 5 digits only");
  }
  else if (customerForm.zipplus4.value.length != 0 && customerForm.zipplus4.value.length != 4)
  {
    isValid = false;
    window.alert("ZIP Plus 4 must be empty or 4 digits only");
  }

  return isValid;
}

/**
 * Validate that the email address is well formed.
 */
function validateEmailAddress(customerForm)
{
  var isValid = true;
  var emailAddr = customerForm.emailaddress.value;

  // first make sure an at-sign is found
  var atPos = emailAddr.indexOf("@");
  if (atPos == -1)
  {
    isValid = false;
  }
  else
  {
    // next, make sure there is at least one domain separator
    // following the at-sign
    var dotPos = emailAddr.indexOf(".", atPos + 1);
    if (dotPos == -1)
    {
      isValid = false;
    }
  }

  if (!isValid)
  {
    window.alert("Your e-mail address must be in the format of username@domain.name");
  }
  return isValid;
}
