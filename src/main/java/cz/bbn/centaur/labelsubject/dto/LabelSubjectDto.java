package cz.bbn.cerberus.labelsubject.dto;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.label.dto.LabelDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.time.LocalDate;
import java.util.Set;

@Getter
@Setter
@ToString
public class LabelSubjectDto implements Serializable {

    private Long id;
    private String subjectId;
    private LabelDto labelDto;
    private LocalDate date;
    private String text;
    private Integer integer;
    private Set<ItemDto> tableSet;

}
