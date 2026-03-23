
## change nest_state
```sql
ALTER TABLE NESTS 
MODIFY COLUMN nest_state ENUM('F','C','I','pP','pD','H','notA','D','P')
DEFAULT NULL COMMENT '<b>F</b> (found), <b>C</b> (collected), <b>I</b> (Incubated, active nest, warm eggs), <b>(p)P</b> [(possibly)Predated], <b>(p)D</b> [(possibly)Deserted, cold eggs], <b>H</b> (hatched, received hatched chicks), <b>notA</b> (not Active, nest marks removed), <b>D</b> (Deserted), <b>P</b> (Predated)';


```


# Add harness size column to CAPTURES  2025-04-17 11:43:49

```sql
ALTER TABLE CAPTURES
ADD COLUMN harness DOUBLE COMMENT 'harness diameter (mm)' AFTER tag_action;
```


# Add column for capture failure.2025-04-20 12:34:11
```sql

ALTER TABLE `NESTS`
  MODIFY COLUMN `cap_fail`
    ENUM('tech','no-show','spooked', 'escaped', 'other')
    NULL
    COMMENT 'Capture failure: technical, no-show,escaped, spooked or other. If `other` specify in the comments'
    AFTER `trap_on`;


```