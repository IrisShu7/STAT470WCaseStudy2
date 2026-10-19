from pathlib import Path
import json, numpy as np, pandas as pd
import statsmodels.api as sm
from sklearn.model_selection import train_test_split,KFold
from sklearn.metrics import mean_squared_error,mean_absolute_error,r2_score
from statsmodels.stats.diagnostic import het_breuschpagan
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from scipy import stats
O=Path(__file__).resolve().parent
source=O/'case-study2-data.csv'
if not source.exists(): source=Path('/workspace/scratch/fd9c35bd5e76/upload/case-study2-data (1).csv')
x=pd.read_csv(source,encoding='cp1252')
x['Amazon_Price']=pd.to_numeric(x.Amazon_Price.str.replace(r'[$, ]','',regex=True),errors='coerce')
x['age']=pd.to_numeric(x.Ages.str.replace('½','.5').str.extract(r'Ages_(\d+(?:\.\d+)?)')[0],errors='coerce')
d=x[(x.Amazon_Price>0)&(x.Pieces>0)].copy()
train,test=train_test_split(d,test_size=.2,random_state=123)
names=['Raw price: pieces','Log price: log pieces','Log price: structure','Log price: theme','Log price: theme + construction']
def design(a,b,k):
 A=pd.DataFrame(index=a.index);B=pd.DataFrame(index=b.index)
 nums={'Pieces':('pieces',False)} if k==0 else {'Pieces':('log_pieces',True)}
 if k>=2: nums.update({'Minifigures':('log1p_minifigures',True),'age':('minimum_age',False)})
 if k==4: nums.update({'Pages':('log_pages',True),'Unique_Pieces':('log_unique_pieces',True)})
 for col,(label,log) in nums.items():
  aa=a[col].copy();bb=b[col].copy()
  if log: aa=np.log1p(aa) if col=='Minifigures' else np.log(aa);bb=np.log1p(bb) if col=='Minifigures' else np.log(bb)
  med=aa.median();A[label]=aa.fillna(med);B[label]=bb.fillna(med)
  if aa.isna().sum()>=5:A[label+'_missing']=aa.isna().astype(int);B[label+'_missing']=bb.isna().astype(int)
 cats=['Year'] if k>=2 else []
 if k==2: cats+=['Size']
 if k>=3: cats+=['Theme']
 for col in cats:
  aa=a[col].fillna('Unknown').astype(str);bb=b[col].fillna('Unknown').astype(str)
  if col=='Theme':
   common=set(aa.value_counts()[lambda s:s>=15].index);aa=aa.where(aa.isin(common),'Other');bb=bb.where(bb.isin(common),'Other')
  levels=sorted(aa.unique());aa=pd.Categorical(aa,categories=levels);bb=pd.Categorical(bb,categories=levels)
  da=pd.get_dummies(aa,prefix=col,drop_first=True,dtype=float);db=pd.get_dummies(bb,prefix=col,drop_first=True,dtype=float);da.index=a.index;db.index=b.index
  A=pd.concat([A,da],axis=1);B=pd.concat([B,db],axis=1)
 return sm.add_constant(A.astype(float),has_constant='add'),sm.add_constant(B.astype(float),has_constant='add')
def fitpred(a,b,k):
 A,B=design(a,b,k);y=a.Amazon_Price if k==0 else np.log(a.Amazon_Price)
 m=sm.OLS(y,A).fit();smear=1 if k==0 else float(np.exp(m.resid).mean());pred=m.predict(B)
 if k:pred=np.exp(pred)*smear
 return np.asarray(pred),m,A,B,smear
cv=KFold(5,shuffle=True,random_state=123);rows=[]
for k,name in enumerate(names):
 p=np.empty(len(train))
 for tr,va in cv.split(train):p[va]=fitpred(train.iloc[tr],train.iloc[va],k)[0]
 rows.append({'Model':name,'CV_RMSE':float(np.sqrt(mean_squared_error(train.Amazon_Price,p))),'CV_MAE':float(mean_absolute_error(train.Amazon_Price,p))})
best=int(np.argmin([r['CV_RMSE'] for r in rows]));pred,m,A,B,smear=fitpred(train,test,best)
metrics={'n_total':len(x),'n_observed_price':int(x.Amazon_Price.notna().sum()),'n_eligible':len(d),'n_train':len(train),'n_test':len(test),'selected':names[best],'selected_index':best,'CV':rows,'test_RMSE':float(np.sqrt(mean_squared_error(test.Amazon_Price,pred))),'test_MAE':float(mean_absolute_error(test.Amazon_Price,pred)),'test_R2':float(r2_score(test.Amazon_Price,pred)),'smearing':smear,'train_R2_log':m.rsquared,'test_mean_baseline_RMSE':float(np.sqrt(mean_squared_error(test.Amazon_Price,np.repeat(train.Amazon_Price.mean(),len(test))))),'BP_p':float(het_breuschpagan(m.resid,A)[1]),'max_cook':float(m.get_influence().cooks_distance[0].max())}
rob=m.get_robustcov_results(cov_type='HC3');ci=rob.conf_int();co=pd.DataFrame({'Term':A.columns,'Estimate':rob.params,'HC3_SE':rob.bse,'CI_low':ci[:,0],'CI_high':ci[:,1],'p':rob.pvalues});co.to_csv(O/'coefficients.csv',index=False)
pd.DataFrame(rows).to_csv(O/'model_comparison.csv',index=False)
pd.DataFrame({'Item_Number':test.Item_Number,'Set_Name':test.Set_Name,'Observed':test.Amazon_Price,'Predicted':pred}).to_csv(O/'held_out_predictions.csv',index=False)
# Sensitivity: retain primary fit; assess removing highest-Cook training set only.
cooks=m.get_influence().cooks_distance[0];idx=int(np.nanargmax(cooks));p2=fitpred(train.drop(train.index[idx]),test,best)[0];metrics['sensitivity_removed_set']=str(train.iloc[idx].Set_Name);metrics['sensitivity_test_RMSE']=float(np.sqrt(mean_squared_error(test.Amazon_Price,p2)))
(O/'results.json').write_text(json.dumps(metrics,indent=2));print(json.dumps(metrics,indent=2));print(co.to_string(index=False))
plt.rcParams.update({'font.size':10,'axes.spines.top':False,'axes.spines.right':False})
f,ax=plt.subplots(figsize=(8,4));ax.barh([r['Model'] for r in rows],[r['CV_RMSE'] for r in rows],facecolor='white',edgecolor='black');ax.invert_yaxis();ax.set_xlabel('5-fold cross-validation RMSE ($), training data');f.tight_layout();f.savefig(O/'model_comparison.png',dpi=220);plt.close(f)
f,ax=plt.subplots(figsize=(6,5));ax.scatter(test.Amazon_Price,pred,facecolors='none',edgecolors='black',s=22);lim=max(max(pred),test.Amazon_Price.max());ax.plot([0,lim],[0,lim],'k--');ax.set(xlabel='Observed Amazon price ($)',ylabel='Predicted Amazon price ($)',title='Held-out predictions');f.tight_layout();f.savefig(O/'predictions.png',dpi=220);plt.close(f)
f,axs=plt.subplots(1,2,figsize=(10,4));axs[0].scatter(m.fittedvalues,m.resid,facecolors='none',edgecolors='black',s=12);axs[0].axhline(0,color='black',linestyle='--');axs[0].set(xlabel='Fitted log price',ylabel='Residual',title='Training residuals');sm.qqplot(m.resid,line='s',ax=axs[1],marker='o',markerfacecolor='white',markeredgecolor='black');axs[1].set_title('Normal Q–Q plot');axs[1].lines[-1].set_color('black');f.tight_layout();f.savefig(O/'diagnostics.png',dpi=220);plt.close(f)
