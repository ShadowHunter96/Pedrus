package cz.bbn.cerberus.opportunity.dto;

import lombok.Getter;
import lombok.Setter;


@Getter
@Setter
public class OpportunitySimpleDto {

    private String id;
    private String name;
    private String description;
    private String subject;
    private OpportunityState state;
    private Integer progress;
    private Boolean deleted;
    private Long userId;

}
