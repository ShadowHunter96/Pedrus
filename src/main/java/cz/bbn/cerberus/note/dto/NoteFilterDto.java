package cz.bbn.cerberus.note.dto;

import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDateTime;
import java.util.Set;


@Getter
@Setter
public class NoteFilterDto {

    private NoteTypeEnum noteTypeEnum;
    private String entityId;
    private Set<String> permission;
    private Boolean showArchived;

    private String text;
    private LocalDateTime createdFrom;
    private LocalDateTime createdTo;

    private UserDto createdBy;

    private int page;
    private int size;
}
