package cz.bbn.cerberus.email.dto;

import com.vaadin.flow.component.upload.receivers.MemoryBuffer;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;
import org.simplejavamail.outlookmessageparser.model.OutlookMessage;

@Getter
@Setter
public class EmailImportDto {

    private SubjectDto customer;
    private DomainEnum domain;
    private SimpleItemDto pickedEntity;
    private OutlookMessage outlookMessage;
    private MemoryBuffer memoryBuffer;
}
