-- CRC-32
drop function if exists export_tools.get_crc32(data bytea);
create or replace function export_tools.get_crc32(data bytea) returns int4 as $$
    declare crc_table bit(32)[];
    declare res bit(32) = x'ffffffff'::bit(32);
    begin
        -- fill crc table
        crc_table = array[]::int[];
        for i in 1..256 loop
            crc_table = array_append(crc_table, (i-1)::bit(32));
            for j in 0..7 loop
                crc_table[i] = case
                    when (crc_table[i] & 1::bit(32))::int = 1
                    then (crc_table[i] >> 1) # X'EDB88320'::bit(32)
                    else crc_table[i] >> 1
                end;
            end loop;
        end loop;

        for i in 0..(length(data)-1) loop
            res = crc_table[((res # (get_byte(data, i))::bit(32)) & x'000000ff'::bit(32))::int4+1] # (res >> 8);
        end loop;
        return (res # x'ffffffff'::bit(32))::int4;
    end;
$$ language plpgsql;
--
-- select * from export_tools.get_crc32(decode('313233343536373839'::text, 'hex')); -- -873187034 (0xCBF43926)
-- -- select length(decode('313233343536373839'::text, 'hex')), length('313233343536373839'::bytea);
--
-- select export_tools.get_crc32(export_tools.json_bit_varying_arr_to_bytea(array[b'01001001', b'01000101', b'01001110', b'01000100']::bit varying[]))::bit(32);
-- -- 10101110 01000010 01100000 10000010
-- -- 10101110 01000010 01100000 10000010
-- select export_tools.get_crc32('IEND'::bytea)::bit(32);

drop domain if exists uint2;
create domain uint2 as int4 check(value >= 0 and value <= 65535);
-- select 100000::uint2;

-- drop domain if exists uint4;
-- create domain uint4 as int8 check(value >= 0 and value <= 4294967295);
-- select 100000::uint4;

-- ansii85


drop function if exists export_tools.reverse_bit_array(arr bit varying(8)[]);
create or replace function export_tools.reverse_bit_array(arr bit varying(8)[]) returns bit varying(8)[] as $$
    begin
        return (select array_agg(i.el order by i.index desc) from unnest(arr) with ordinality i(el, index));
    end;
    $$ language plpgsql;

select export_tools.bytea_to_bit_varying_arr('This is a test!');

drop function if exists export_tools.encode_ansii85(data bit varying(8)[]);
create or replace function export_tools.encode_ansii85(data bit varying(8)[]) returns bit varying(8)[] as $$
declare add_length int4;
declare temp_array bit varying(8)[];
declare res_array bit varying(8)[];
declare curr_element bit(32);
declare temp_value int4;
declare index int4;
begin
--     raise notice 'array_length % %', array_length(data, 1), 4 - (array_length(data, 1) % 4);
    add_length = 4 - (array_length(data, 1) % 4);
    data = data || export_tools.bytea_to_bit_varying_arr(export_tools.form_empty_bytea(add_length));

    res_array = array[]::bit varying(8)[];
    curr_element = 0::bit(32);
    for index in (select generate_subscripts(data, 1)) loop
        curr_element = (curr_element << 8) | ((data[index])::bit(32) >> 24);
--         raise notice 'charCodeAt % % % %', index % 4 = 0, index, data[index]::bit(32)::int, curr_element::int4;
        if index % 4 = 0 then
            temp_array = array[]::bit varying(8)[];
            if curr_element::int4 = 0 then
                temp_array = array[122::bit(8)::bit varying(8)];
            else
                for i in 1..5 loop
                    temp_value = curr_element::int4 % 85;
                    curr_element = ((curr_element::int4 - temp_value) / 85)::bit(32);
--                     raise notice 'temp_value % % %', i, temp_value, curr_element::bit(32)::int;
                    temp_array =  array[(temp_value + 33)::bit(8)::bit varying(8)] || temp_array;
                end loop;
            end if;
            curr_element = 0::bit(32);
            res_array = res_array || temp_array;
        end if;
        raise notice 'array % % % % %', export_tools.show_bit_arr_as_int_arr(res_array), array_length(data, 1), add_length, array_length(data, 1) - add_length, export_tools.show_bit_arr_as_int_arr(res_array[:array_length(res_array, 1) - add_length]);
    end loop;
    return export_tools.bytea_to_bit_varying_arr('<~') || res_array[:array_length(res_array, 1) - add_length] || export_tools.bytea_to_bit_varying_arr('~>');
end;
$$ language plpgsql;

select export_tools.json_bit_varying_arr_to_bytea(
    export_tools.encode_ansii85(
        export_tools.bytea_to_bit_varying_arr('This is a test!') -- <~<+oue+DGm>@3BZ'F*&Q~>
        )
    );

select export_tools.json_bit_varying_arr_to_bytea(
    export_tools.encode_ansii85(
        export_tools.bytea_to_bit_varying_arr('1234567890') -- <~0etOA2)[BQ3A:~>
        )
    );

drop function if exists export_tools.decode_ansii85(data bit varying(8)[]);
create or replace function export_tools.decode_ansii85(data bit varying(8)[]) returns bit varying(8)[] as $$
    declare add_length int4;
    declare res_array bit varying(8)[];
    declare curr_element int8;
    declare index int4;
    declare temp_str text;
    begin
        temp_str = export_tools.bytea_to_text(export_tools.json_bit_varying_arr_to_bytea(data));
        temp_str = case when substring(temp_str for 2) = '<~' and substring(temp_str from length(temp_str) - 1) = '~>' then substring(temp_str from 3 for length(temp_str) - 4) else temp_str end;
        temp_str = replace(regexp_replace(temp_str, '\s', '', 'g'), 'z', '!!!!!');
        data = export_tools.bytea_to_bit_varying_arr(export_tools.text_to_bytea(temp_str));
        add_length = 5 - (array_length(data, 1) % 5);
        data = data || export_tools.bytea_to_bit_varying_arr(export_tools.form_empty_bytea(add_length, 'u'));

        res_array = array[]::bit varying(8)[];
        curr_element = 0;
        for index in (select generate_subscripts(data, 1)) loop
            curr_element = curr_element * 85 + ((data[index])::bit(8)::int - 33);
            if index % 5 = 0 then
                res_array = res_array || array[
                    curr_element::bit(32)::bit(8),
                    (curr_element::bit(32) << 8)::bit(8),
                    (curr_element::bit(32) << 16)::bit(8),
                    (curr_element::bit(32) << 24)::bit(8)
                    ]::bit varying(8)[];
                curr_element = 0;
            end if;
        end loop;

       return res_array[:array_length(res_array, 1) - add_length];
    end;
$$ language plpgsql;

select export_tools.json_bit_varying_arr_to_bytea(
    export_tools.decode_ansii85(
        export_tools.bytea_to_bit_varying_arr('<~<+oue+DGm>@3BZ''F*&Q~>') -- 'This is a test!'
        )
    );