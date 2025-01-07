package cz.bbn.cerberus.document.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.orderedlayout.FlexComponent;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.textfield.TextField;
import cz.bbn.cerberus.commons.component.ui.AppHelp;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.document.DocumentObjectEnum;
import cz.bbn.cerberus.document.dto.DocumentFilterDto;
import cz.bbn.cerberus.documenttype.dto.DocumentTypeDto;
import cz.bbn.cerberus.translation.Transl;

import java.util.List;

public class DocumentFilterComponent extends FormLayout {

    private TextField name;
    private TextField fileType;
    private ComboBox<DocumentTypeDto> documentType;
    private Checkbox showOnlyUnlinked;
    private Checkbox showOnlyDeleted;
    private ComboBox<DocumentObjectEnum> documentObjectType;
    private TextField documentObjectId;

    private final List<DocumentTypeDto> documentTypeDtoList;
    private final Button search;
    private final AppHelp filterInfo;

    public DocumentFilterComponent(List<DocumentTypeDto> documentTypeDtoList, Button search, AppHelp filterInfo) {
        this.documentTypeDtoList = documentTypeDtoList;
        this.search = search;
        this.filterInfo = filterInfo;
        initComponent();
    }

    private void initComponent() {
        name = new TextField(Transl.get("Name"));
        this.add(name);

        fileType = new TextField(Transl.get("Type"));
        this.add(fileType);

        documentType = new ComboBox<>(Transl.get("Document type"));
        DocumentTypeDto documentTypeDto = new DocumentTypeDto();
        documentTypeDto.setName(Transl.get("Show all"));
        documentTypeDtoList.add(0, documentTypeDto);
        documentType.setItems(documentTypeDtoList);
        documentType.setValue(documentTypeDto);
        documentType.setItemLabelGenerator(DocumentTypeDto::getName);
        this.add(documentType);

        documentObjectType = new ComboBox<>(Transl.get("Object type"));
        documentObjectType.setItems(DocumentObjectEnum.values());
        documentObjectType.setValue(DocumentObjectEnum.ALL);
        documentObjectType.setItemLabelGenerator(documentObjectEnum -> Transl.get(documentObjectEnum.name()));
        this.add(documentObjectType);

        documentObjectId = new TextField(Transl.get("Object id"));
        this.add(documentObjectId);

        showOnlyUnlinked = new Checkbox(Transl.get("Show only unlinked"));
        this.add(showOnlyUnlinked);

        showOnlyDeleted = new Checkbox(Transl.get("Show only deleted"));
        this.add(showOnlyDeleted);
        HorizontalLayout buttonLayout = new HorizontalLayout();

        buttonLayout.setAlignItems(FlexComponent.Alignment.CENTER);
        buttonLayout.add(search, filterInfo);


        this.add(buttonLayout);

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(this);
    }

    public DocumentFilterDto getDocumentFilterDto() {
        DocumentFilterDto filterDto = new DocumentFilterDto();
        filterDto.setName(name.getValue());
        filterDto.setFileType(fileType.getValue());
        if (documentType.getValue() != null) {
            filterDto.setDocumentType(documentType.getValue().getId());
        } else {
            filterDto.setDocumentType("");
        }
        filterDto.setShowOnlyUnlinked(showOnlyUnlinked.getValue());
        filterDto.setShowOnlyDeleted(showOnlyDeleted.getValue());
        filterDto.setObjectType(documentObjectType.getValue());
        filterDto.setObjectId(documentObjectId.getValue());
        return filterDto;
    }

}
