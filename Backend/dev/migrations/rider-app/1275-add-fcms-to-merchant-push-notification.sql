-- please run this while doing Master and Prod Release

INSERT INTO atlas_app.merchant_push_notification (
    fcm_notification_type, key, merchant_id, merchant_operating_city_id, title, body, language, created_at, updated_at
)
SELECT
    'CALL_SERVICE_DOWN',
    'CALL_SERVICE_DOWN',
    moc.merchant_id,
    moc.id,
    'Try Direct Calling',
    'Please Use Direct Calling Option for Better Connectivity.',
    'ENGLISH',
    CURRENT_TIMESTAMP,
    CURRENT_TIMESTAMP
FROM
    atlas_app.merchant_operating_city moc;