package cz.bbn.cerberus.contactperson.dto;

import cz.bbn.cerberus.contactpersontype.dto.ContactPersonTypeDto;
import cz.bbn.cerberus.phoneprefix.dto.PhonePrefixDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;
import org.apache.commons.lang3.StringUtils;

import java.io.Serializable;
import java.util.List;


@Getter
@Setter
@ToString
public class ContactPersonDto implements Serializable {

    private String id;

    private String firstName;
    private String lastName;
    private String description;
    private String email;

    private PhonePrefixDto phonePrefixDto;
    private String phone;

    private PhonePrefixDto phonePrefix2Dto;
    private String phone2;

    private String otherContacts;
    private ContactPersonTypeDto contactPersonType;
    private String contactPersonPosition;
    private Boolean deleted;
    private List<TypeByObject> typeByObjectList;

    public String getName() {
        return StringUtils.trimToEmpty(firstName).concat(" ").concat(StringUtils.trimToEmpty(lastName));
    }

}
