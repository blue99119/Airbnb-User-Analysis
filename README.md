# Airbnb-User-Analysis
The analysis is for the below kaggle competition.
https://www.kaggle.com/competitions/airbnb-recruiting-new-user-bookings/overview


一開始用R Studio來進行EDA與資料清理，但建制模型時發現電腦無法負荷，故模型階段改為使用Python。
跑完Ｒscript會產出df_all_combined_train.csv與df_all_combined_test.csv兩文件，再將此文件上傳至Python進行模型預測。
另，由於時間限制，尚未使用cross validation，日後會再將此部分加上，成績應可再提升。
目前版本所跑出成績為：private 0.87003, public 0.86543。

還有一版本增加了session data，但預測成績較差：private 0.86223, public 0.85793。
