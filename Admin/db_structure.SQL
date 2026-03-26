

-- setup 
CREATE DATABASE IF NOT EXISTS FIELD_2026_NOLAatDUMMERSEE; 
USE FIELD_2026_NOLAatDUMMERSEE; 
-- show grants for '***field_db_user***';
-- GRANT SELECT, SHOW VIEW ON FIELD_2026_NOLAatDUMMERSEE.* TO '***field_db_user***'@'%';

DROP TABLE IF EXISTS AUTHORS;
CREATE TABLE IF NOT EXISTS AUTHORS (
  Name char(50)                DEFAULT NULL COMMENT 'Your full name.',
  author varchar(3)            DEFAULT NULL COMMENT 'Your initials. Use two or three letters. This is what you will use when entering data.',
  START date                   DEFAULT NULL COMMENT 'Start of work (YYYY-MM-DD)',
  STOP date                    DEFAULT NULL COMMENT 'End of work (YYYY-MM-DD)',
  gps_id int(2)                DEFAULT NULL COMMENT 'Personal GPS ID',
  radio_id int(2)              DEFAULT NULL COMMENT 'Personal radio ID',
  cam_id int(2)                DEFAULT NULL COMMENT 'Personal camcorder ID',
  remarks varchar(50)          DEFAULT NULL COMMENT 'Please provide any relevant information, such as your ratings in cooking, chess, or poker.'
) ENGINE=ARIA;

DROP TABLE IF EXISTS CAPTURES;
CREATE TABLE CAPTURES (
  species varchar(50)          DEFAULT NULL COMMENT '4 letter code, e.g. NOLA',
  date date                    DEFAULT NULL COMMENT 'Date caught YYYY-MM-DD',
  cap_start time               DEFAULT NULL COMMENT 'hh:mm capture start',
  caught    time               DEFAULT NULL COMMENT 'hh:mm time caught ',
  released time                DEFAULT NULL COMMENT 'hh:mm time released',
  capture_meth char(1)         DEFAULT NULL COMMENT 'T (Trap), M (Mistnet), O (Other)',
  nest VARCHAR(7)              DEFAULT NULL COMMENT '<kbd>Nest ID</kbd>, see NESTS table',
  form_id int(3)               DEFAULT NULL COMMENT 'Capture form ID',
  author varchar(3)            DEFAULT NULL COMMENT 'Initials of the bander (see authors table).',
  gps_id int(2)                DEFAULT NULL COMMENT 'GPS ID (1-12)',
  gps_point int(4)             DEFAULT NULL COMMENT 'Waypoint number of the capture location',
  
  sex_observed ENUM('M','F','U')         DEFAULT NULL COMMENT 'The observed sex: <kbd>F</kbd>(female),<kbd>M</kbd>(male),<kbd>U</kbd>(unknown)',
  ID varchar(255)              DEFAULT NULL COMMENT 'Metal ring number.',

  UL_in varchar(10)               DEFAULT NULL COMMENT 'color band at capture,before handling (leave blank if not changed)',
  UR_in varchar(10)               DEFAULT NULL COMMENT 'color band at capture,before handling (leave blank if not changed)',
  LL_in varchar(10)               DEFAULT NULL COMMENT 'color band at capture,before handling (leave blank if not changed)',
  LR_in varchar(10)               DEFAULT NULL COMMENT 'color band at capture,before handling (leave blank if not changed)',

  UL varchar(10)               DEFAULT NULL COMMENT 'color band at release',
  UR varchar(10)               DEFAULT NULL COMMENT 'color band at release',
  LL varchar(10)               DEFAULT NULL COMMENT 'color band at release',
  LR varchar(10)               DEFAULT NULL COMMENT 'color band at release',

  tagID_in varchar(10)         DEFAULT NULL COMMENT  'tagID at capture, before handling (leave blank if not changed)',
  tagID varchar(10)            DEFAULT NULL COMMENT  'tagID',
  tagType varchar(10)          DEFAULT NULL COMMENT  'D (druid), G (Global messenger)',
  tag_action varchar(10)       DEFAULT NULL COMMENT 'on (tag deployed) or off(tag removed)',
  harness DOUBLE               DEFAULT NULL COMMENT 'harness diameter (mm)',
  
  recapture BOOLEAN            DEFAULT NULL COMMENT '1(yes), 0(no)',
  
  tarsus DOUBLE                DEFAULT NULL COMMENT 'Length (mm)',
  culmen DOUBLE                DEFAULT NULL COMMENT 'Length (mm)',
  total_head DOUBLE            DEFAULT NULL COMMENT 'Length (mm)',
  crest DOUBLE                 DEFAULT NULL COMMENT 'Length (mm)',
  wing DOUBLE                  DEFAULT NULL COMMENT 'Length (mm)',
  weight DOUBLE                DEFAULT NULL COMMENT 'Body mass (g)',
  wt_w_tag BOOLEAN             DEFAULT NULL COMMENT '0(no) or 1(yes).Was the body weight measured with the tag on?',
  samples    varchar(255)      DEFAULT NULL COMMENT 'Comma delimited: F(fecal), C(cloaca_swab), W(blood whole), S(blood smear), P(blood plasma)',
  
  cam_id    varchar(255)       DEFAULT NULL COMMENT 'Camera ID',
  photo_start varchar(255)     DEFAULT NULL COMMENT 'ID photo start',
  photo_end varchar(255)       DEFAULT NULL COMMENT 'ID photo end',


  
  dead BOOLEAN                 DEFAULT NULL COMMENT '1(yes). Write in the comments the cause of death.',
  comments varchar(255)        DEFAULT NULL COMMENT 'Comments',
  
  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'Validation flag. Do not edit this column !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY ID (ID),
  KEY tagID (tagID),
  KEY COMBO (UL,LL,UR,LR),
  KEY gps (gps_id,gps_point)
) ENGINE=ARIA;

CREATE TABLE IF NOT EXISTS RESIGHTINGS (
  species varchar(50)         DEFAULT NULL COMMENT '4 letter code, e.g. NOLA.',
  author varchar(3)            DEFAULT NULL COMMENT 'Initials of the observer (see authors table).',
  gps_id int(2)                DEFAULT NULL COMMENT 'GPS ID',
  gps_point_start int(3)       DEFAULT NULL COMMENT 'GPS waypoint for the start of the behaviour (required)',
  gps_point_stop int(3)        DEFAULT NULL COMMENT 'GPS waypoint for the stop of the behaviour (optional)',

  UL varchar(10)               DEFAULT NULL COMMENT 'color band',
  UR varchar(10)               DEFAULT NULL COMMENT 'color band',
  LL varchar(10)               DEFAULT NULL COMMENT 'color band',
  LR varchar(10)               DEFAULT NULL COMMENT 'color band',
  has_tag TINYINT(1)           DEFAULT NULL  COMMENT '1 if tag is visible',

  behaviour TEXT               DEFAULT NULL COMMENT 'description of the behaviour' ,

  comments varchar(500)        DEFAULT NULL COMMENT 'Comments',
  
  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY COMBO (UL,LL,UR,LR),
  KEY gps1 (gps_id,gps_point_start),
  KEY gps2 (gps_id,gps_point_stop)
) ENGINE=ARIA;

CREATE TABLE IF NOT EXISTS GPS_POINTS (
  gps_id int(2) NOT NULL        COMMENT 'gps id',
  gps_point int(10) NOT NULL    COMMENT 'gps point',
  datetime_ datetime NOT NULL   COMMENT 'gps date-time (CET)',
  lat double NOT NULL           COMMENT 'latitude',
  lon double NOT NULL           COMMENT 'longitude',
  ele double NOT NULL           COMMENT 'elevation',
  pk int(10) NOT NULL           AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY gps (gps_id,gps_point),
  KEY datetime_ (datetime_)
) ENGINE=ARIA;

CREATE TABLE IF NOT EXISTS GPS_TRACKS (
  gps_id int(2) NOT NULL        COMMENT 'gps id',
  seg_id int(10) NOT NULL       COMMENT 'segment id',
  seg_point_id int(10) NOT NULL COMMENT 'segment point id',
  datetime_ datetime NOT NULL   COMMENT 'gps date-time (CET)',
  lat double NOT NULL           COMMENT 'latitude',
  lon double NOT NULL           COMMENT 'longitude',
  ele float NOT NULL            COMMENT 'elevation',
  pk int(10) NOT NULL           AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY gps (gps_id,seg_id),
  KEY datetime_ (datetime_)
) ENGINE=ARIA;


DROP TABLE IF EXISTS NESTS;
CREATE TABLE NESTS (
  author varchar(3)            DEFAULT NULL COMMENT 'Initials (see authors file)',
  nest varchar(7)              DEFAULT NULL COMMENT 'Nest ID: SXYY, Where S is Species(the first letter of the species name), X=your GPS number, YY=your running nest number',
  
  nest_state ENUM('F','C','I','pP','pD','H','notA','D','P')
DEFAULT NULL COMMENT '<b>F</b> (found), <b>C</b> (collected), <b>I</b> (Incubated, active nest, warm eggs), <b>(p)P</b> [(possibly)Predated], <b>(p)D</b> [(possibly)Deserted, cold eggs], <b>H</b> (hatched, received hatched chicks), <b>notA</b> (not Active, nest marks removed), <b>D</b> (Deserted), <b>P</b> (Predated)',
  
  hatch_state varchar(50) DEFAULT NULL COMMENT 'No. of eggs(1-4) followed by egg state (N-no sign, S-star, C-crack,CC-multiple or a big crack). <br> E.g. 4N, 2N2S, 1N1S1C1CC ',
  
  bird_inc ENUM('M','F','U','E') DEFAULT NULL COMMENT ' M(male), F(female), U(unknown), E(nest empty, no one seen); this is usually done from the distance', 
  
  trap_on ENUM('Y') DEFAULT NULL COMMENT 'Was a trap installed on the nest? Y (yes) or leave blank.', 

  cap_fail ENUM('tech','no-show','spooked', 'escaped', 'other') NULL COMMENT 'Capture failure: technical, no-show,escaped, spooked or other. If `other` specify in the comments',


  gps_id int(2)                DEFAULT NULL COMMENT 'GPS ID (recorded when nest is found)',
  gps_point int(3)             DEFAULT NULL COMMENT 'GPS waypoint (recorded when nest is found)',
  date date                    DEFAULT NULL COMMENT 'Date (YYYY-MM-DD) when the nest was checked.',
  time_appr time               DEFAULT NULL COMMENT 'Time (HH:MM) when the nest was checked.',
  clutch_size int(1)           DEFAULT NULL COMMENT 'Clutch size (number of eggs in the nest excluding hatched chicks)',
  brood_size int(1)            DEFAULT NULL COMMENT 'Number of hatched chicks',
  f_UL varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  f_UR varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  f_LL varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  f_LR varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  m_UL varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  m_UR varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  m_LL varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  m_LR varchar(10)             DEFAULT NULL COMMENT 'color band, only enter during nest checks not at capture time!',
  behav varchar(50)            DEFAULT NULL COMMENT 'Behaviour of the incubating bird (comma separated): INC=Incubating, DF=Driving Flight, BW=Distraction Display (e.g. Broken Wing) or O=Other behaviours (describe in comments)',
  comments varchar(500)        DEFAULT NULL COMMENT 'Comments',
  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY nest (nest),
  KEY gps (gps_id,gps_point)
) ENGINE=Aria ;


DROP TABLE IF EXISTS EGGS;
CREATE TABLE EGGS (
  nest varchar(7)               DEFAULT NULL COMMENT '<kbd>Nest ID</kbd>, see NESTS table',
  date date                     DEFAULT NULL COMMENT 'Date (YYYY-MM-DD) when the nest was checked.',
  egg_id int(1)                 DEFAULT NULL COMMENT 'The id of the egg (1-4); only relevant for location = L',  
  location ENUM('F', 'L')       DEFAULT NULL COMMENT 'Indicates if the egg is in the field (F) or in the lab (L).',
  float_angle int(3)            DEFAULT NULL COMMENT 'Egg floating angle (degrees).',    
  surface int(3)                DEFAULT NULL COMMENT 'Egg surface (mm).',    
  weight DECIMAL(6,2)           DEFAULT NULL COMMENT 'Egg weight in grams.',
  
  state  VARCHAR(50)        NULL DEFAULT NULL COMMENT "The state of the egg: normal (checked, nothing found), star, crack, hatched, fertile, broken, dead_embryo, killed",
  action VARCHAR(50)        NULL DEFAULT NULL COMMENT "The action performed: in_incubator, in_hatcher, out_hatcher, tissue, blood, delivered.",

  
  comments varchar(500)         DEFAULT NULL COMMENT 'Comments.',
  nov int(1) unsigned zerofill  DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY nest (nest),
  KEY date (date)
) ENGINE=Aria ;

DROP TABLE IF EXISTS CHICKS;
CREATE TABLE CHICKS (
  
  date      date               DEFAULT NULL COMMENT 'Date (YYYY-MM-DD).',
  location ENUM('N', 'F', 'H') DEFAULT NULL COMMENT 'Chick capture location: N(Nest), F(Free chick - out of the nest), H(Hatcher).',
  caught    time               DEFAULT NULL COMMENT 'hh:mm time caught ',
  released  time               DEFAULT NULL COMMENT 'hh:mm time released',
  recapture TINYINT(1)         DEFAULT NULL COMMENT '1 if recaptured, 0 otherwise',
  nest      varchar(7)         DEFAULT NULL COMMENT '<kbd>Nest ID</kbd>, see NESTS table',
  nest_delivered  varchar(7)   DEFAULT NULL COMMENT '<kbd>Nest ID</kbd>, delivery nest',

  author  varchar(3)           DEFAULT NULL COMMENT 'Initials (see authors file)',
  ID      varchar(255)         DEFAULT NULL COMMENT 'Metal ring number.',   
  tarsus  DOUBLE               DEFAULT NULL COMMENT 'Length (mm)',  
  weight  DOUBLE               DEFAULT NULL COMMENT 'Body mass (g)',
  sample  varchar(255)         DEFAULT NULL COMMENT 'Comma delimited: F(fecal), C(cloaca_swab), W(blood whole), S(blood smear), P(blood plasma)',
  dead TINYINT(1)               DEFAULT NULL COMMENT '1 if dead.',
  comments  varchar(500)       DEFAULT NULL COMMENT 'Comments.',

  nov int(1) unsigned zerofill DEFAULT NULL COMMENT 'No validation. Do not edit !',
  pk int(10) NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY nest (nest),
  KEY date (date)
) ENGINE=Aria ;

DROP TABLE IF EXISTS SAMPLES;
CREATE TABLE SAMPLES (
  ID VARCHAR(50) NOT NULL,
  date_collect DATE NOT NULL            COMMENT 'Date the sample was collected',
  notes_field VARCHAR(255) DEFAULT NULL COMMENT 'Notes from field observation',
  tissue_collected TINYINT(1) DEFAULT 0 COMMENT '1 if tissue collected, 0 otherwise',
  embryo_score TINYINT UNSIGNED DEFAULT NULL COMMENT 'Categorical embryo score from 0 to 9',
  date_processed DATE DEFAULT NULL      COMMENT 'Date the sample was processed (if applicable)',
  author VARCHAR(3) DEFAULT NULL        COMMENT 'Initials of collector/processor',
  notes_lab VARCHAR(255) DEFAULT NULL   COMMENT 'Notes from lab examination',
  PRIMARY KEY (ID)
) ENGINE=Aria;


-- COMBOS
  DROP TABLE IF EXISTS COMBOS;
  CREATE TABLE COMBOS (
    UL varchar(10)      DEFAULT NULL COMMENT  'Upper left',
    UR varchar(10)      DEFAULT NULL COMMENT  'Upper right',
    LL varchar(10)      DEFAULT NULL COMMENT  'Lower left',
    LR varchar(10)      DEFAULT NULL COMMENT  'Lower right',
    
    pk int(10) NOT NULL AUTO_INCREMENT,
    PRIMARY KEY (pk),
    KEY COMBO (UL,LL,UR,LR)
  ) ENGINE=ARIA DEFAULT CHARACTER SET utf8mb4 COLLATE=utf8mb4_general_ci;


  INSERT INTO COMBOS (UL, UR) 
    VALUES ('R,R','R,G'),('R,B','R,G'),('R,G','R,G'),('R,N','R,G'),('R,Y','R,G'),('B,R','R,G'),('B,B','R,G'),('B,G','R,G'),('B,N','R,G'),('B,Y','R,G'),('G,R','R,G'),('G,B','R,G'),('G,G','R,G'),('G,N','R,G'),('G,Y','R,G'),('N,R','R,G'),('N,B','R,G'),('N,G','R,G'),('N,N','R,G'),('N,Y','R,G'),('Y,R','R,G'),('Y,B','R,G'),('Y,G','R,G'),('Y,N','R,G'),('Y,Y','R,G'),('R,R','Y,R'),('R,B','Y,R'),('R,G','Y,R'),('R,N','Y,R'),('R,Y','Y,R'),('B,R','Y,R'),('Y,Y','Y,R'),('B,B','Y,R'),('B,G','Y,R'),('B,N','Y,R'),('B,Y','Y,R'),('G,R','Y,R'),('G,B','Y,R'),('G,G','Y,R'),('G,N','Y,R'),('G,Y','Y,R'),('N,R','Y,R'),('N,B','Y,R'),('N,G','Y,R'),('N,N','Y,R'),('N,Y','Y,R'),('Y,R','Y,R'),('Y,B','Y,R'),('Y,G','Y,R'),('Y,N','Y,R'),('Y,Y','G,B'),('R,R','G,B'),('R,B','G,B'),('R,G','G,B'),('R,N','G,B'),('R,Y','G,B'),('B,R','G,B'),('B,B','G,B'),('B,G','G,B'),('B,N','G,B'),('B,Y','G,B'),('G,R','G,B'),('G,B','G,B'),('G,G','G,B'),('G,N','G,B'),('G,Y','G,B'),('N,R','G,B'),('N,B','G,B'),('N,G','G,B'),('N,N','G,B'),('N,Y','G,B'),('Y,R','G,B'),('Y,B','G,B'),('Y,G','G,B'),('Y,N','G,B'),('R,R','G,G'),('R,B','G,G'),('R,G','G,G'),('R,N','G,G'),('R,Y','G,G'),('B,R','G,G'),('B,B','G,G'),('B,G','G,G'),('B,N','G,G'),('B,Y','G,G'),('G,R','G,G'),('G,B','G,G'),('G,G','G,G'),('G,N','G,G'),('G,Y','G,G'),('N,R','G,G'),('N,B','G,G'),('N,G','G,G'),('N,N','G,G'),('N,Y','G,G'),('Y,R','G,G'),('Y,B','G,G'),('Y,G','G,G'),('Y,N','G,G'),('Y,Y','G,G'),('B,B','Y,B'),('B,G','Y,B'),('B,N','Y,B'),('B,R','Y,B'),('B,Y','Y,B'),('G,B','Y,B'),('G,R','Y,B'),('N,G','R,N'),('R,B','Y,B'),('R,G','Y,B'),('R,N','Y,B'),('R,R','Y,B'),('R,Y','Y,B'),('Y,N','R,Y'),('Y,Y','R,Y'),('Y,Y','Y,G');
  UPDATE COMBOS SET LR = 'M';


-- CAPTURES_ARCHIVE
RENAME TABLE
  FIELD_2025_NOLAatDUMMERSEE.CAPTURES_ARCHIVE
TO
  FIELD_2026_NOLAatDUMMERSEE.CAPTURES_ARCHIVE;


  INSERT INTO CAPTURES_ARCHIVE 
      (date, sex_observed, crest, ID, UL, UR, LL, LR, tagID, tagType)
  SELECT 
      date,
      sex_observed,
      crest,
      ID,
      UL,
      UR,
      LL,
      LR,
      tagID,
      'D' as tagType
  FROM FIELD_2025_NOLAatDUMMERSEE.CAPTURES;

-- todo_white_list
CREATE TABLE IF NOT EXISTS todo_white_list (
  `variable` VARCHAR(50)    DEFAULT NULL COMMENT 'Column name from TODO table to whitelist',
  `value`    VARCHAR(50)    DEFAULT NULL COMMENT 'Value for that variable (e.g. nest, color combo)'
) ENGINE=ARIA;



-- VIEWS

  DROP   VIEW IF EXISTS nest_locations;

  CREATE VIEW nest_locations AS 
  SELECT DISTINCT nest, lat, lon from NESTS n join `GPS_POINTS` g on n.gps_id = g.gps_id AND n.gps_point = g.gps_point;


  DROP   VIEW IF EXISTS CAPTURES_ALL;
  CREATE VIEW CAPTURES_ALL AS 
    SELECT * FROM CAPTURES_ARCHIVE UNION
    SELECT date, sex_observed, crest, ID, UL, UR, LL, LR, tagID, tagType FROM CAPTURES
    ;
