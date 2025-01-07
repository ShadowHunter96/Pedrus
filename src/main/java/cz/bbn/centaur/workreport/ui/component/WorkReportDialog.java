package cz.bbn.cerberus.workreport.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.ProjectPhaseActivityDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import org.apache.commons.lang3.SerializationUtils;

import java.util.List;

public class WorkReportDialog extends AppDialog {

    private final WorkReportDto dto;
    private final WorkReportDto originalDto;
    private final ProjectPhaseActivityDto projectPhaseActivityDto;
    private final List<Double> durationList;
    private final WorkReportSaveListener listener;
    private final boolean enableActions;
    private final boolean enableDateChange;

    public WorkReportDialog(WorkReportDto dto, ProjectPhaseActivityDto projectPhaseActivityDto,
                            List<Double> durationList, WorkReportSaveListener listener,
                            boolean enableActions, boolean enableDateChange) {
        this.dto = dto;
        this.originalDto = SerializationUtils.clone(dto);
        this.projectPhaseActivityDto = projectPhaseActivityDto;
        this.durationList = durationList;
        this.listener = listener;
        this.enableActions = enableActions;
        this.enableDateChange = enableDateChange;
        initDialogComponent();
    }

    private void initDialogComponent() {
        Binder<WorkReportDto> binder = new Binder<>();
        WorkReportDataComponent workReportDataComponent = new WorkReportDataComponent(projectPhaseActivityDto,
                durationList, binder, listener, dto, false, enableDateChange, null);
        this.setTitle(Transl.get("Work report"));
        this.setContent(workReportDataComponent);

        workReportDataComponent.setWidth("auto");

        binder.setBean(dto);

        this.addCloseButton();
        if (enableActions && dto.getApprovementDto() == null) {
            Button submitButton = VaadinComponents.getSubmitButton();
            submitButton.addClickListener(e -> listener.save(binder, this, originalDto, true, null));
            this.addSubmitButton(submitButton);
        } else {
            workReportDataComponent.setComponentEnabled(false);
        }
    }
}
