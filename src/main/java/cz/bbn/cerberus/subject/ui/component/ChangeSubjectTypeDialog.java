package cz.bbn.cerberus.subject.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.checkbox.Checkbox;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.interfaces.SaveAction;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import cz.bbn.cerberus.translation.Transl;
import org.apache.commons.lang3.SerializationUtils;

public class ChangeSubjectTypeDialog extends AppDialog {

    private final SubjectDto subjectDto;
    private final SaveAction<SubjectDto> saveAction;
    private final AppEnv appEnv;

    private SubjectDto originalDto;

    private Binder<SubjectDto> binder = new Binder<>();

    public ChangeSubjectTypeDialog(SubjectDto subjectDto,
                                   SaveAction<SubjectDto> saveAction, AppEnv appEnv) {
        this.subjectDto = subjectDto;
        this.saveAction = saveAction;
        this.originalDto = SerializationUtils.clone(subjectDto);
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent(){
        setTitle(Transl.get("Subject type"));
        HorizontalLayout horizontalLayout = new HorizontalLayout();

        Checkbox customer = new Checkbox(Transl.get("Customer"));
        binder.forField(customer).bind(SubjectDto::getCustomer, SubjectDto::setCustomer);
        horizontalLayout.add(customer);

        Checkbox supplier = new Checkbox(Transl.get("supplier"));
        binder.forField(supplier).bind(SubjectDto::getSupplier, SubjectDto::setSupplier);
        horizontalLayout.add(supplier);
        binder.setBean(subjectDto);

        setContent(horizontalLayout);
        addCloseButton();
        Button submit = VaadinComponents.getSubmitButton();
        submit.addClickListener(buttonClickEvent -> {
            if(!Boolean.TRUE.equals(subjectDto.getCustomer()) && !Boolean.TRUE.equals(subjectDto.getSupplier())){
                ErrorNotification.show("Either the customer or the supplier must be selected", appEnv);
            }else {
                saveAction.saveItem(subjectDto, originalDto);
                this.close();
            }
        });
        addButtons(submit);
    }
}
