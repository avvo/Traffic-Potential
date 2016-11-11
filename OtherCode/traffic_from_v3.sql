select a.ad_market_id
, ad_specialty pa
, b.ad_market_county_name county
, b.ad_market_state_name state
, count(distinct session_id) sessions
from dm.ad_attribution_v3_all a
join dm.ad_market_dimension b on b.ad_market_id = a.ad_market_id
where event_date between '2016-06-01' and '2016-08-31'
and lpv_medium <> 'utm_medium=SEM'
group by 1,2,3,4
