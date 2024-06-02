-- ONLY FOR LOCAL TESTING For driver with new vehicle_variant

-- INSERT into person for driver with vehicle_variant
INSERT INTO atlas_driver_offer_bpp.person (id, first_name, middle_name, last_name, role, gender, identifier_type, email, password_hash, mobile_number_encrypted, mobile_number_hash, mobile_country_code, identifier, is_new, device_token, description, created_at, updated_at, rating, merchant_id, onboarded_from_dashboard, total_earned_coins,used_coins, unencrypted_mobile_number, merchant_operating_city_id)
VALUES ('favorit-bike-00000000000000000000000', 'Sherlock', NULL, 'Holmes', 'DRIVER', 'MALE', 'MOBILENUMBER', 'sholmes@gmail.com', NULL, '0.1.0|2|NPTk9iMIhAkUrqZ+Tkmxv2vPi6aro7JHMJrc0GumUyQ2+5r6SdsXd0hcNlv9zJ0IK8PygCdDkXamWo8aaw==', '\xb0a56e902b973e6cf231520c2acbda9b44947dd3a88fb0daacd23d68082c6362', '+91', NULL, true, 'favorit-bike-00000000000device-token', NULL, '2022-04-12 15:15:42.222142+00', '2022-04-12 15:15:42.222142+00', NULL,'favorit0-0000-0000-0000-00000favorit', false, 0, 0, '8888888888', 'favorit0-0000-0000-0000-00000000city');


-- INSERT into driver_information for driver with vehicle_varian
INSERT INTO atlas_driver_offer_bpp.driver_information (driver_id, active, on_ride, created_at, updated_at, enabled)
VALUES ('favorit-bike-00000000000000000000000', false, false, '2022-04-12 15:15:42.280941+00', '2022-04-12 15:15:42.280941+00', true);


-- INSERT into driver_location
INSERT INTO atlas_driver_offer_bpp.driver_location (driver_id, lat, lon, point, created_at, coordinates_calculated_at)
VALUES ('favorit-bike-00000000000000000000000', 10.0739, 76.2733, '0101000020E6100000CC7F48BF7D1153402B598638D6252440', '2022-04-12 15:15:42.27825+00', now());


-- INSERT into driver_stats
INSERT INTO atlas_driver_offer_bpp.driver_stats (driver_id, idle_since)
VALUES ('favorit-bike-00000000000000000000000', '2022-04-12 15:15:42.283174+00');


-- INSERT into registration_token
INSERT INTO atlas_driver_offer_bpp.registration_token (id, auth_medium, auth_type, auth_value_hash, token, verified, auth_expiry, token_expiry, attempts, entity_id, entity_type, info, created_at, updated_at, merchant_id)
VALUES ('favorit-bike-00000000000000000000000', 'SMS', 'OTP', '1234', 'favorit-bike-00000000000000000-token', true, 3, 365, 3, 'favorit-bike-00000000000000000000000', 'USER                                ', NULL, now (), now (), 'favorit0-0000-0000-0000-00000favorit');


-- INSERT into vehicle with vehicle_variant
INSERT INTO atlas_driver_offer_bpp.vehicle (driver_id, capacity, category, make, model, size, variant, color, energy_type, registration_no, registration_category, vehicle_name, vehicle_class, merchant_id, created_at, updated_at)
VALUES ('favorit-bike-00000000000000000000000', 1, NULL, NULL, 'Splendor', NULL, 'BIKE', 'Yellow', NULL, '4815', NULL, NULL, '2WN', 'favorit0-0000-0000-0000-00000favorit', '2022-04-12 15:15:42.233691+00', '2022-04-12 15:15:42.233691+00');


-- INSERT into fare_product
WITH bike_fp_ids AS (
    SELECT fare_policy_id
    FROM atlas_driver_offer_bpp.fare_product
    WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'
    AND vehicle_variant = 'BIKE'
)
UPDATE atlas_driver_offer_bpp.fare_policy SET per_minute_ride_extra_time_charge=1
WHERE id IN (SELECT fare_policy_id FROM bike_fp_ids);


-- INSERT into bekcn_config
INSERT INTO
    atlas_driver_offer_bpp.beckn_config (
        id,
        domain,
        gateway_url,
        registry_url,
        subscriber_id,
        subscriber_url,
        merchant_id,
        merchant_operating_city_id,
        unique_key_id,
        created_at,
        updated_at,
        vehicle_category,
        on_search_ttl_sec,
        on_select_ttl_sec,
        on_init_ttl_sec,
        on_confirm_ttl_sec,
        on_track_ttl_sec,
        on_status_ttl_sec,
        on_cancel_ttl_sec,
        on_update_ttl_sec,
        payment_params_json,
        cancellation_fee_amount,
        cancellation_fee_percentage
    )
  VALUES (
        'dd22a05d-29a3-42c8-9c8d-2de340f9b611',
        'MOBILITY',
        'http://localhost:8015/v1',
        'http://localhost:8020',
        'NAMMA_YATRI',
        'http://localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        'favorit0-0000-0000-0000-00000favorit',
        null,
        'localhost:8013/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52',
        now(),
        now(),
        'MOTORCYCLE',
        120,
        120,
        120,
        120,
        120,
        120,
        120,
        120,
        '{"bankAccNumber": "xyz@upi","bankCode": "xyz"}',
        10,
        0
    );


-- INSERT into vehicle_details
INSERT INTO atlas_driver_offer_bpp.vehicle_details (id, make, model, vehicle_variant, ac_available)
VALUES ('fa9ba789-e0f6-4318-88c8-8b24fb5eeae2', 'Splendor', 'Yamaha', 'BIKE', true);