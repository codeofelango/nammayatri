--Only for Local
--REQUIRED TO ADD CONFIGS FOR ALL CITIES AND VEHICLE SERVICE TIERS FOR PROD!!!!!!!!!
INSERT INTO atlas_driver_offer_bpp.cancellation_and_now_show_charge_configs(
    id,
    currency,
    free_cancellation_time_seconds,
    max_cancellation_charge,
    max_waiting_time_at_pickup_seconds,
    merchant_id,
    merchant_operating_city_id,
    min_cancellation_charge,
    per_metre_cancellation_charge,
    per_minute_cancellation_charge,
    percentage_of_ride_fare_to_be_charged,
    vehicle_service_tier_type,
    created_at,
    updated_at)
SELECT
    md5(random()::text || clock_timestamp()::text || city.id::text || vehicle_service_tier_type)::uuid,
    'INR',
    300,
    100.00,
    300,
    city.merchant_id,
    city.id,
    10.00,
    0.5,
    0.5,
    10.00,
    vehicle_service_tier_type,
    now(),
    now()
FROM
    atlas_driver_offer_bpp.merchant_operating_city AS city
CROSS JOIN
    (VALUES
        ('COMFY'),
        ('ECO'),
        ('PREMIUM'),
        ('SUV'),
        ('AUTO_RICKSHAW'),
        ('HATCHBACK'),
        ('SEDAN'),
        ('TAXI'),
        ('TAXI_PLUS'),
        ('PREMIUM_SEDAN'),
        ('BLACK'),
        ('BLACK_XL'),
        ('BIKE'),
        ('AMBULANCE_TAXI'),
        ('AMBULANCE_TAXI_OXY'),
        ('AMBULANCE_AC'),
        ('AMBULANCE_AC_OXY'),
        ('AMBULANCE_VENTILATOR'),
        ('SUV_PLUS')
    ) AS vehicle_service_tiers(vehicle_service_tier_type);
