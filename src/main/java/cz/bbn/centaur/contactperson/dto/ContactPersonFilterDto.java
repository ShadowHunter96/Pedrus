package cz.bbn.cerberus.contactperson.dto;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.util.List;

@Getter
@Setter
public class ContactPersonFilterDto {

    private String id;
    private String firstName;
    private String lastName;
    private String email;
    private String phone;
    private String contactPersonType;
    private String projectId;
    private String subjectId;
    private boolean showDeleted;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
