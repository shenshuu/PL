; (1 (2 (3 4)))
     
; (1 (2 (3 4)))
;     |               
;     |                 
;   _____ _____       _____ _____  
;  |     |     |     |     | \   |
;  |  1  |  .--|---> |  .  |  \  |
;  |     |     |     |  |  |   \ |
;   -----------       -----------  
;                       |
;                       |
;                     _____ _____       _____ _____ 
;                    |     |     |     |     | \   |
;                    |  2  |  .--|---> |  .  |  \  |
;                    |     |     |     |  |  |   \ |
;                     -----------       ----------- 
;                                         |
;                                         |
;                                       _____ _____       _____ _____ 
;                                      |     |     |     |     | \   |
;                                      |  3  |  .--|---> |  .  |  \  |
;                                      |     |     |     |  |  |   \ |
;                                       -----------       ----------- 
;                                                           |
;                                                           |
;                                                           4
;
;    (1 (2 (3 4)))
;     /       \
;    /         \
;   1       (2 (3 4))
;            /   \
;           /     \
;          2     (3 4)
;                 /  \
;                3   4