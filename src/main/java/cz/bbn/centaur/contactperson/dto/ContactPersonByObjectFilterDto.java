package cz.bbn.cerberus.contactperson.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class ContactPersonByObjectFilterDto {

    private int page;
    private int size;
    private List<Sort.Order> orderList;
    private ContactPersonObjectTypeEnum objectType;
    private String objectId;
    private boolean showDeleted = false;
}
