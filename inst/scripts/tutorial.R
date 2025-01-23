# Tutorial Script for using

# creating a connection using newconnHandler()
# In this instance the the user will have admin permissions for testing purposes.

conn_handler <- SigRepo::newConnHandler(dbname = 'sigrepo',
                        host = 'montilab.bu.edu',
                        port = 3306,
                        user = 'guest',
                        password = 'guest')


# grabbing data path

data_path <- system.file("data", package="SigRepo")

# adding a signature into the database

# searching for a signature within the data base

SigRepo::searchSignature(conn_handler, signature_name = 'LLFS_Transcriptomic_AGS_OmS')



# need to change privelages I think because the guest does not have acces to this
SigRepo::addSignature(conn_handler, OmS_MDA_AhR)



# Retrieving an omic signature object

SigRepo::getSignature(conn_handler, signature_name = 'OmS_MDA_AhR')

# deleting an omic Signature object

SigRepo::deleteSignature(conn_handler, signature_name = 'OmS_MDA_AhR')


# updating an omic Signatute object 

SigRepo::updateSignature(conn_handler, signature_name = '')

dbListTables(conn)
