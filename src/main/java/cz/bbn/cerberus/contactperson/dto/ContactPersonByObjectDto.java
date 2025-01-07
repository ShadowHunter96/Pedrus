package cz.bbn.cerberus.contactperson.dto;

import cz.bbn.cerberus.phoneprefix.dto.PhonePrefixDto;
import lombok.Getter;
import lombok.Setter;
import org.apache.commons.lang3.StringUtils;

@Getter
@Setter
public class ContactPersonByObjectDto {

    private String id;
    private String firstName;
    private String lastName;
    private String email;
    private String phone;
    private String addedObjectId;
    private ContactPersonObjectTypeEnum objectType;
    private PhonePrefixDto phonePrefixDto;

    public String getName() {
        return StringUtils.trimToEmpty(firstName).concat(" ").concat(StringUtils.trimToEmpty(lastName));
    }
}
