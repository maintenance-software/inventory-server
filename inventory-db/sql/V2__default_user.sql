INSERT INTO oauth2_client (
      id
    , secret
    , authorized_grant_types
    , redirect_uri
    , access_token_validity
    , refresh_token_validity
    , allowed_scope
    , auto_approve
    , auth_method
    , auth_alg
    , keys_uri
    , keys
    , id_token_algs
    , user_info_algs
    , request_obj_algs
    , sector_identifier
) VALUES (
      'app'
    , 'appsecret'
    , '{authorization_code}'
    , '{http://localhost:3000/auth/page/inventoty-auth-provider/callback,http://192.168.0.100:3000/auth/page/inventoty-auth-provider/callback,http://192.168.99.100:3000/auth/page/inventoty-auth-provider/callback}'
    , 3600, 7200, '{openid,profile,address,email}'
    , false, 'client_secret_basic'
    , null
    , null
    , '[]'
    , null
    , null
    , null
    , 'localhost'
) ON CONFLICT DO NOTHING;

WITH row AS (
    INSERT INTO t_person (
      first_name
      , last_name
      , document_type
      , document_id
      , created_date
      , modified_date
    ) VALUES (
      'Admin'
      , 'Admin'
      , 'NIT'
      , '777'
      , current_timestamp
      , null
    ) ON CONFLICT DO NOTHING RETURNING person_id
) INSERT INTO t_user (
    username
    , email
    , password
    , status
    , locale
    , expiration
    , new_password_required
    , person_id
    , created_date
    , modified_date
) (SELECT 'admin',
          'admin@dummy.com',
          '$2b$06$nyXH6ETvP3PjcJUbwXLTNuJd6.yS21ovKMNQ9/Z.ZR3w1qLKIlNuC',
          'ACTIVE',
          'es_BO',
          false,
          false,
          person_id,
          current_timestamp,
          null
   FROM row
) ON CONFLICT DO NOTHING RETURNING user_id;
