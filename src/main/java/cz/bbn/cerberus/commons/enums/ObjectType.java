package cz.bbn.cerberus.commons.enums;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public enum ObjectType {

    OPPORTUNITY, OFFER, CONTRACT, PROJECT,
    ASSET, SUBJECT, INVOICE, CONTACT_PERSON, ANY;

    public static List<ItemDto> getEntityTypeListForApi(){
        List<ItemDto> list = new ArrayList<>();
        Arrays.stream(new ObjectType[]{
                OPPORTUNITY,
                OFFER,
                PROJECT,
                SUBJECT,
                CONTRACT
        }).toList().forEach(objectType ->
                list.add(new ItemDto(objectType.name(), Transl.getByLang(objectType.name(), Transl.DEFAULT_LANG))));
        return list;
    }

    public static ObjectType[] getEntityType(){
        return new ObjectType[]{
                OPPORTUNITY,
                OFFER,
                PROJECT,
                SUBJECT,
                CONTRACT,
        };
    }

}
