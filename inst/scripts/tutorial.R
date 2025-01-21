# Tutorial Script for using

# creating a connection using newconnHandler()
# In this instance the the user will have admin permissions for testing purposes.

conn_handler <- SigRepo::newConnHandler(dbname = 'sigrepo',
                        host = 'montilab.bu.edu',
                        port = 3306,
                        user = 'guest',
                        password = 'guest')




# adding a signature into the database

# searching for a signature within the data base

SigRepo::searchSignature(conn_handler, signature_name = 'LLFS_Transcriptomic_AGS_OmS')




SigRepo::addSignature(conn, 'LLFS_Transcriptomic_AGS_OmS')



# Retrieving an omic signature object

SigRepo::getSignature(conn_handler, signature_name = '')

# deleting an omic Signature object

# updating an omic Signatute object 

library

build_site()
