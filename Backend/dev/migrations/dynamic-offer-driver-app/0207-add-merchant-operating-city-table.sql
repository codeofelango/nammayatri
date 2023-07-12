CREATE TABLE atlas_driver_offer_bpp.merchant_operating_city (
    id character(36) NOT NULL PRIMARY KEY,
    merchant_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.merchant (id),
    city character varying(255) NOT NULL
);

INSERT INTO atlas_driver_offer_bpp.merchant_operating_city VALUES ('da4e23a5-3ce6-4c37-8b9b-41377c3c1a58','favorit0-0000-0000-0000-00000favorit', 'BANGALORE');
INSERT INTO atlas_driver_offer_bpp.merchant_operating_city VALUES ('da3e23a5-3ce6-4c37-8b9b-41377c3c1a59','nearest-drivers-testing-organization', 'KOCHI');

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);

UPDATE atlas_driver_offer_bpp.merchant_service_usage_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.merchant_service_usage_config.merchant_id = merchant_operating_city.merchant_id;

UPDATE atlas_driver_offer_bpp.merchant_service_config
SET merchant_operating_city_id = merchant_operating_city.id
FROM atlas_driver_offer_bpp.merchant_operating_city
WHERE atlas_driver_offer_bpp.merchant_service_config.merchant_id = merchant_operating_city.merchant_id;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ALTER COLUMN merchant_operating_city_id SET NOT NULL;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config ALTER COLUMN merchant_operating_city_id SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config DROP CONSTRAINT merchant_service_usage_config_pkey;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config DROP CONSTRAINT merchant_service_config_pkey;

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config ADD PRIMARY KEY (merchant_operating_city_id);
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config ADD PRIMARY KEY (merchant_operating_city_id, service_name);

ALTER TABLE atlas_driver_offer_bpp.merchant_service_usage_config DROP COLUMN merchant_id;
ALTER TABLE atlas_driver_offer_bpp.merchant_service_config DROP COLUMN merchant_id;


ALTER TABLE atlas_driver_offer_bpp.registration_token ADD COLUMN merchant_operating_city_id TEXT NOT NULL DEFAULT 'da4e23a5-3ce6-4c37-8b9b-41377c3c1a58';
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);
ALTER TABLE atlas_driver_offer_bpp.search_request ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);
ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);
ALTER TABLE atlas_driver_offer_bpp.search_request_special_zone ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);
ALTER TABLE atlas_driver_offer_bpp.search_try ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);
ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN merchant_operating_city_id character(36) REFERENCES atlas_driver_offer_bpp.merchant_operating_city(id);