select attribution_date
, marketid
,case when pt='serp' or pt='profile' then 'core' else 'noncore' end as pt
,cup
,sum(sl_acv) sl_acv
,sum(da_acv) da_acv
,sum(potential_sl_acv) potential_sl_acv
,sum(potential_da_acv) potential_da_acv
,sum(sl_imps) sl_imps
,sum(da_imps) da_imps
,sum(potential_sl_imps) potential_sl_imps
,sum(potential_da_imps) potential_sl_imps
from dm.potential_acv_marketid
where attribution_date  = adddate(process_date, -1)
and process_date between '2016-06-02' and '2016-09-01'
and channel='Other'
group by 1,2,3,4
