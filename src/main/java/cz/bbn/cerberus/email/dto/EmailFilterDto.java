package cz.bbn.cerberus.email.dto;

import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.time.LocalDateTime;
import java.util.List;

@Getter
@Setter
public class EmailFilterDto {

    private String subject;
    private String sender;
    private LocalDateTime fromDateTime;
    private LocalDateTime toDateTime;
    private Integer noOfAttachments;
    private SubjectDto customer;
    private DomainEnum entityType;
    private String entityId;
    private SimpleItemDto item;
    private String searchAll;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
