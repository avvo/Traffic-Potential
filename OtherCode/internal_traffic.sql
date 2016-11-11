with A as
(
select cast(concat(cast(year(a.event_date) as string), lpad(cast(month(a.event_date) as string),2,'0')) as int) as year_month
, a.ad_market_id
, a.state
, a.county
, a.region
, a.specialty_name
, a.parent_specialty_name
, a.block_flag
 , a.page
  , b.medium
  , count(distinct a.session_id) sessions
from
(
select pv.session_id
, pv.event_date
, pv.page_type as page
, amd.ad_market_id
, amd.ad_market_state_name as state
, amd.ad_market_county_name as county
, amd.ad_market_region_name as region
, sd.specialty_name
, sd.parent_specialty_name
, amd.ad_market_block_flag as block_flag
from src.page_view pv
left join dm.specialty_dimension sd on pv.specialty_id = sd.specialty_id
left join dm.geography_dimension gd on gd.geo_id = pv.location_id
left join dm.ad_market_dimension amd on amd.specialty_id = pv.specialty_id and amd.ad_region_id = gd.sales_region_id
where pv.event_date between '2016-06-01' and '2016-08-31'
) a
join
(
select event_date
, session_id
, medium
from
(
select event_date
, session_id
, case when url like '%utm_campaign=brand%' then 'brand'
when url like '%utm_campaign=pls%'  then 'pls'
when url like '%utm_content=sgt%' then 'network'
when url like '%utm_campaign=adblock%' then 'adblock' else 'nonsem' end as medium
,row_number() OVER (PARTITION BY session_id ORDER BY gmt_timestamp ASC) rankNum
from src.page_view
WHERE session_id IS NOT NULL
AND persistent_session_id IS NOT NULL
AND render_instance_guid IS NOT NULL
and event_date between '2016-06-01' and '2016-08-31'
) c
where rankNum=1
and medium!='network'
) b on b.event_date = a.event_date and b.session_id = a.session_id
where ad_market_id>0 and ad_market_id is not null
group by 1,2,3,4,5,6,7,8,9,10
)
, B as
(
select b.ad_market_id
, b.ad_market_specialty_name
,sum( case when a.ad_inventory_type='Sponsored Listing' then sellable_count else 0 end) as sl
,sum(case when a.ad_inventory_type='Display' then sellable_count else 0 end) as da
,sum( case when a.ad_inventory_type='Sponsored Listing' then list_price else 0 end) as sl_price
, sum(case when a.ad_inventory_type='Display' then list_price else 0 end) as da_price
from src.nrt_ad_inventory a
join dm.ad_market_dimension b on b.specialty_id=a.specialty_id and a.ad_target_id=b.ad_market_id and b.ad_market_active_flag='Y'
group by 1,2
)

select A.*
,B.sl
,B.sl_price
,B.da
,B.da_price
from A join B on A.ad_market_id = B.ad_market_id and A.specialty_name=B.ad_market_specialty_name
where A.medium='nonsem'
