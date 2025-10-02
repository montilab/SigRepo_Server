
# SigRepo_Server

**SigRepo_Server** is a reproducible workflow that enables users to
launch a `SigRepo` database server and interact with it via a
user-friendly, web-based interface built with R Shiny.

The server architecture composes of three core components: a **MySQL
database**, **RESTful APIs**, and a **Shiny application**.

- The **MySQL database** is responsible for initializing and managing
  the **SigRepo** database, which stores biological signatures and
  collections along with their associated metadata.  
- The **RESTful APIs** provide endpoints to configure the database
  schema, import essential reference tables (such as organisms,
  platforms, and sample types, etc.), and lastly, store or retrieve
  difexp objects linked to specific signatures.
- Finally, the **Shiny application** offers an interactive interface
  that integrates with the MySQL database and APIs, allowing users to
  perform operations through a set of functions provided by our R client
  package, <a href="https://github.com/montilab/SigRepo"
  target="_blank"><strong>SigRepo</strong></a>.

# Web Interface

An interactive R Shiny dashboard has been developed to make it easy for
users to explore and interact with our **SigRepo** database. To access
the signatures and collections stored in our database,
<a target="_blank" href="https://sigrepo.org/">VISIT OUR WEBSITE</a> to
create an account or <a href="mailto:sigrepo@bu.edu">CONTACT US</a> to
be added.

Need more help? Click on the links below for additional guides:

- <a
  href="https://montilab.github.io/SigRepo_Server/articles/create_user_account.html"
  target="_blank">Register an account to access the SigRepo database</a>
- <a href="https://montilab.github.io/SigRepo/index.html"
  target="_blank">Connect and interact with the SigRepo database through
  the SigRepo R client</a>
- <a
  href="https://montilab.github.io/SigRepo_Server/articles/install_sigrepo_locally.html"
  target="_blank">Initialize a SigRepo database instance on your local
  machine</a>

Any questions or issues? Please report them on our
<a href="https://github.com/montilab/SigRepo_Server/issues"
target="_blank">github issues</a>.
