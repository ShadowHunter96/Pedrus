package cz.bbn.cerberus.note.dto;

import cz.bbn.cerberus.note.NoteTypeEnum;
import cz.bbn.cerberus.user.dto.UserDto;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.io.Serializable;
import java.time.LocalDateTime;

@Getter
@Setter
@ToString
public class NoteDto implements Serializable {
    private Long id;
    private String note;
    private LocalDateTime date;
    private NoteTypeEnum type;
    private Boolean priority;
    private String entityId;
    private Boolean archived;
    private UserDto userDto;
}
