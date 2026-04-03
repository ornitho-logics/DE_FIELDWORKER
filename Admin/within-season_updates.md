


# sex colum in resightings

```sql
ALTER TABLE RESIGHTINGS
ADD COLUMN   sex ENUM('M','F','U')  DEFAULT NULL COMMENT 'Sex: <kbd>F</kbd>(female),<kbd>M</kbd>(male),<kbd>U</kbd>(unknown)' AFTER LR;
```
# tag_eol table



```sql
CREATE TABLE tags_eol (

  tagID varchar(10)  DEFAULT NULL COMMENT  'tagID',
  eol TINYINT(1)     DEFAULT NULL COMMENT '1 if tag is end of life and has to be retrieved',
  pk int(10)       NOT NULL AUTO_INCREMENT,
  PRIMARY KEY (pk),
  KEY tagID (tagID)
) ENGINE=Aria 



  INSERT INTO tags_eol 
      (tagID)
  SELECT distinct  tagID
  FROM CAPTURES_ALL;



```


# nest summary table TODO
* nest
* F_ID
* M_ID
* F_tag
* M_tag
* 'clutch collect? Y/N'