package cz.bbn.cerberus.labelsubject.factory;

import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.label.dto.LabelDto;
import cz.bbn.cerberus.label.factory.LabelFactory;
import cz.bbn.cerberus.label.persistance.LabelEntity;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.labelsubject.persistance.LabelSubjectEntity;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class LabelSubjectFactory {

    private LabelSubjectFactory() {
    }

    public static LabelSubjectDto fromEntity(LabelSubjectEntity entity) {
        LabelSubjectDto dto = new LabelSubjectDto();
        dto.setId(entity.getId());
        dto.setSubjectId(entity.getSubjectId());
        dto.setLabelDto(LabelFactory.fromEntity(entity.getLabelEntity()));
        switch (dto.getLabelDto().getType()) {
            case STRING -> dto.setText(entity.getValue());
            case NUMBER -> dto.setInteger(Integer.valueOf(entity.getValue()));
            case DATE -> dto.setDate(LocalDate.parse(entity.getValue()));
            case TABLE -> {
                Set<ItemDto> itemDtoSet = new HashSet<>();
                Arrays.stream(entity
                                .getValue()
                                .split(";"))
                        .toList().forEach(s -> itemDtoSet.add(new ItemDto(s)));
                dto.setTableSet(itemDtoSet);
            }
        }
        return dto;
    }

    public static LabelSubjectDto fillDto(LabelSubjectDto dto, LabelDto labelDto, String subjectId) {
        dto.setLabelDto(labelDto);
        dto.setSubjectId(subjectId);
        return dto;
    }

    public static LabelSubjectEntity fillEntity(LabelSubjectDto dto) {
        LabelSubjectEntity entity = new LabelSubjectEntity();

        LabelEntity labelEntity = new LabelEntity();
        LabelFactory.fillEntity(labelEntity, dto.getLabelDto());
        entity.setLabelEntity(labelEntity);

        switch (dto.getLabelDto().getType()) {
            case STRING -> entity.setValue(dto.getText());
            case NUMBER -> entity.setValue(dto.getInteger().toString());
            case DATE -> entity.setValue(dto.getDate().format(DateTimeFormatter.ISO_LOCAL_DATE));
            case TABLE -> {
                String values = "";
                for (ItemDto itemDto : dto.getTableSet()) {
                    values += itemDto.getName().concat(";");
                }
                entity.setValue(values);
            }
            default -> entity.setValue("");
        }
        entity.setSubjectId(dto.getSubjectId());

        return entity;
    }
}
