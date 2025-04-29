--
-- USE database
--
USE sigrepo;
--
-- USERS TABLE
--
INSERT INTO users (user_name, user_email, user_first, user_last, user_affiliation, user_role, user_password_hashkey, api_key, user_hashkey)
VALUES ('guest', 'guest@montilab.bu.edu', 'guest', 'guest', 'Boston University', 'viewer', '$7$C6..../....eHYZbJCzNjC6iZvxygn4bpFnrj81TY0lqecTTpPWE0C$i5rU2mhxbDfu.tMa3wRzrCbog/vWPMtZJ48yxYkOdAA', '1bcc4d9e4aa18d29098822d7a546241f', '084e0343a0486ff05530df6c705c8bb4'),
('montilab', 'editor@montilab.bu.edu', 'montilab', 'montilab', 'Boston University', 'editor', '$7$C6..../....JQz77Cl18dldr20QC6VbIuRKCHdQ0gzOXMSQ2eAW8k2$ZU6tcCMTXJoODMILcRcqOau8wvpAdLVp.GKA081VYM9', '3e44212a4aa8ac205b34aa26ac5562a6', '3c1b9fe0592958976c855113c52e5ee1');

      
