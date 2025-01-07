package cz.bbn.cerberus.labelsubject.ui.component;

import com.vaadin.flow.component.button.Button;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.label.ui.component.LabelLinkDialog;
import cz.bbn.cerberus.labelsubject.LabelSubjectComponentOperation;
import cz.bbn.cerberus.labelsubject.dto.LabelSubjectDto;
import cz.bbn.cerberus.translation.Transl;

public class LabelSubjectExitsDialog extends AppDialog {

    private final LabelSubjectComponentOperation labelSubjectComponentOperation;
    private final LabelSubjectDto dto;
    private final AppInfiniteGrid<LabelSubjectDto> grid;
    private final LabelLinkDialog dialog;

    public LabelSubjectExitsDialog(LabelSubjectComponentOperation labelSubjectComponentOperation, LabelSubjectDto dto,
                                   AppInfiniteGrid<LabelSubjectDto> grid, LabelLinkDialog dialog) {
        this.labelSubjectComponentOperation = labelSubjectComponentOperation;
        this.dto = dto;
        this.grid = grid;
        this.dialog = dialog;
        initComponent();
    }

    private void initComponent(){
        setTitle(Transl.get("Label exist"));
        setTextAsContent(Transl.get("Label already exists for this customer. Are you sure you want to save this?"));

        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent -> {
            labelSubjectComponentOperation.getSaveAction(dialog, grid, false).saveItem(dto, null);
            this.close();
        });
        addCloseButton();
        addSubmitButton(submit);
    }
}
