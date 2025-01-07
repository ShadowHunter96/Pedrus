package cz.bbn.cerberus.virtualserver.ui.component;

import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.combobox.ComboBox;
import com.vaadin.flow.component.formlayout.FormLayout;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.IntegerField;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.TextValues;
import cz.bbn.cerberus.commons.component.ui.AppInfiniteGrid;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.commons.component.ui.layouts.LayoutUtils;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.enumeration.dto.EnumerationDto;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.virtualserver.VirtualServerComponentOperation;
import cz.bbn.cerberus.virtualserver.dto.HddDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerAction;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerDto;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerNotificationPeriod;
import cz.bbn.cerberus.virtualserver.dto.VirtualServerStatus;

import java.util.ArrayList;
import java.util.List;

public class VirtualServerDialog extends AppDialog {

    private final VirtualServerDto dto;
    private final List<EnumerationDto> subnetList;
    private final HorizontalLayout hddLayout = new HorizontalLayout();
    private final List<Binder<HddDto>> binderList = new ArrayList<>();
    private final VirtualServerComponentOperation componentOperation;
    private final AppInfiniteGrid<VirtualServerDto> grid;
    private final boolean isInf;

    private List<HddDto> hddList;

    public VirtualServerDialog(VirtualServerDto dto, List<EnumerationDto> subnetList,
                               VirtualServerComponentOperation componentOperation,
                               AppInfiniteGrid<VirtualServerDto> grid) {
        this.dto = dto;
        this.hddList = dto.getHddDtoList();
        this.subnetList = subnetList;
        this.componentOperation = componentOperation;
        this.grid = grid;
        this.isInf = componentOperation.getIsInfrastructure();
        if (hddList == null || hddList.isEmpty()) {
            hddList = new ArrayList<>();
            hddList.add(new HddDto());
        }
        for (HddDto hddDto : hddList) {
            Binder<HddDto> hddBinder = new Binder<>();
            hddBinder.setBean(hddDto);
            binderList.add(hddBinder);
        }
        initDialog();
    }

    private void initDialog() {

        VerticalLayout verticalLayout = new VerticalLayout();

        if (dto.getId() == null) {
            setTitle(Transl.get("New virtual server"));
            dto.setOwner(SecurityUtils.getCurrentUserDto());
        } else {
            setTitle(Transl.get("Virtual server"));
        }

        Binder<VirtualServerDto> binder = new Binder<>();

        FormLayout formLayout = new FormLayout();

        if (dto.getId() != null) {
            TextField stringId = new TextField(Transl.get("Id"));
            stringId.setReadOnly(true);
            binder.forField(stringId).bind(VirtualServerDto::getStringId, VirtualServerDto::setStringId);
            formLayout.add(stringId);
        }

        TextField name = new TextField(Transl.get("Name"));
        name.setMaxLength(100);
        binder.forField(name).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(VirtualServerDto::getName, VirtualServerDto::setName);
        formLayout.add(name);

        TextField os = new TextField(Transl.get("OS"));
        os.setMaxLength(100);
        binder.forField(os).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(VirtualServerDto::getOs, VirtualServerDto::setOs);
        formLayout.add(os);

        IntegerField cpu = new IntegerField(Transl.get("CPU"));
        cpu.setMin(0);
        cpu.setMax(100);
        if (isInf) {
            binder.forField(cpu).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(VirtualServerDto::getCpu, VirtualServerDto::setCpu);
            formLayout.add(cpu);
        }

        IntegerField cores = new IntegerField(Transl.get("Cores"));
        cores.setMin(0);
        cores.setMax(100);
        binder.forField(cores).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(VirtualServerDto::getCores, VirtualServerDto::setCores);
        formLayout.add(cores);

        IntegerField ram = new IntegerField(Transl.get("RAM in GB"));
        ram.setMin(0);
        ram.setMax(1000);
        binder.forField(ram).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(VirtualServerDto::getRam, VirtualServerDto::setRam);
        formLayout.add(ram);

        ComboBox<EnumerationDto> subnet = new ComboBox<>(Transl.get("Subnet"));
        subnet.setItems(subnetList);
        subnet.setItemLabelGenerator(EnumerationDto::getName);
        binder.forField(subnet).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                .bind(VirtualServerDto::getSubnet, VirtualServerDto::setSubnet);
        formLayout.add(subnet);

        TextField subnetValue = new TextField(Transl.get("Subnet value"));
        if (dto.getSubnet() != null) {
            subnetValue.setValue(dto.getSubnet().getValue());
        }
        formLayout.add(subnetValue);
        subnetValue.setReadOnly(true);

        subnet.addValueChangeListener(e -> {
            if (e.getValue() != null) {
                subnetValue.setValue(e.getValue().getValue());
            }
        });

        TextField ip = new TextField(Transl.get("IP"));
        TextField owner = new TextField(Transl.get("Requester"));
        TextField status = new TextField(Transl.get("Status"));
        ComboBox<VirtualServerNotificationPeriod> notificationPeriod =
                new ComboBox<>(Transl.get("Notification period"));

        if (dto.getId() != null) {
            ip.setMaxLength(50);
            binder.forField(ip).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(VirtualServerDto::getIp, VirtualServerDto::setIp);
            formLayout.add(ip);

            owner.setValue(dto.getOwner().getName());
            owner.setReadOnly(true);
            formLayout.add(owner);

            status.setValue(Transl.get(dto.getStatus().getValue()));
            status.setReadOnly(true);
            formLayout.add(status);

            if (isInf) {
                notificationPeriod.setItems(VirtualServerNotificationPeriod.values());
                notificationPeriod.setItemLabelGenerator(VirtualServerNotificationPeriod::getTranslatedValue);
                binder.forField(notificationPeriod).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                        .bind(VirtualServerDto::getNotificationPeriod, VirtualServerDto::setNotificationPeriod);
                formLayout.add(notificationPeriod);
            }
        }

        hddLayout.getElement().getStyle().set("align-items", "baseline");

        if (!canEdit()) {
            name.setReadOnly(true);
            os.setReadOnly(true);
            cpu.setReadOnly(true);
            cores.setReadOnly(true);
            ram.setReadOnly(true);
            subnet.setReadOnly(true);
            ip.setReadOnly(true);
        }

        LayoutUtils.setDefaultInfiniteColumnResponsiveSteps(formLayout);
        verticalLayout.add(formLayout);
        generateHddLayout();
        verticalLayout.add(hddLayout);
        setContent(verticalLayout);

        binder.setBean(dto);

        addButtons(binder);

    }

    private boolean canEdit() {
        if (dto.getId() == null) {
            return true;
        }
        return isInf;
    }

    private void generateHddLayout() {
        Button add = VaadinComponents.getButton(VaadinIcon.PLUS.create());
        Button remove = VaadinComponents.getButton(VaadinIcon.MINUS.create());
        if (hddList == null || hddList.isEmpty()) {
            hddList = new ArrayList<>();
            hddList.add(new HddDto());
        }
        add.addClickListener(e -> {
            HddDto hddDto = new HddDto();
            Binder<HddDto> hddBinder = new Binder<>();
            hddBinder.setBean(hddDto);
            hddList.add(hddDto);
            binderList.add(hddBinder);
            recreateHddLayout(add, remove);
        });

        remove.addClickListener(e -> {
            if (hddList.size() > 1) {
                hddList.remove(hddList.size() - 1);
                binderList.remove(binderList.size() - 1);
                recreateHddLayout(add, remove);
            }
        });
        recreateHddLayout(add, remove);
    }

    private void recreateHddLayout(Button addButton, Button removeButton) {
        hddLayout.removeAll();
        if (canEdit()) {
            hddLayout.add(addButton);
        }

        for (int i = 0; i < hddList.size(); i++) {
            HorizontalLayout singleHddLayout = new HorizontalLayout();
            singleHddLayout.setSpacing(false);

            TextField hddName = new TextField(Transl.get("HDD name"));
            hddName.setMaxLength(100);
            binderList.get(i).forField(hddName).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(HddDto::getName, HddDto::setName);
            singleHddLayout.add(hddName);
            hddName.getElement().getStyle().set("margin-right", "3px");

            IntegerField hddSize = new IntegerField(Transl.get("Hdd size in GB"));
            hddSize.setMin(0);
            hddSize.setMax(10000);
            binderList.get(i).forField(hddSize).asRequired(Transl.get(TextValues.CANNOT_BE_EMPTY))
                    .bind(HddDto::getSize, HddDto::setSize);
            singleHddLayout.add(hddSize);

            if (!canEdit()) {
                hddName.setReadOnly(true);
                hddSize.setReadOnly(true);
            }

            hddLayout.add(singleHddLayout);
        }
        if (hddList.size() > 1 && canEdit()) {
            hddLayout.add(removeButton);
        }
    }

    private void addButtons(Binder<VirtualServerDto> binder) {

        addCloseButton();

        if (dto.getStatus() == VirtualServerStatus.CREATING) {
            Button cancelRequest = VaadinComponents.getButton(
                    Transl.get(VirtualServerAction.CANCEL_REQUEST.getAction()), VaadinIcon.RECYCLE.create());
            cancelRequest.addClickListener(e -> componentOperation.cancelRequest(binder.getBean(), this, grid));
            addButtons(cancelRequest);
        }
        if (dto.getStatus() == VirtualServerStatus.RUNNING) {
            Button requestDeleting = VaadinComponents.getButton(
                    Transl.get(VirtualServerAction.REQUEST_DELETING.getAction()), VaadinIcon.RECYCLE.create());
            requestDeleting.addClickListener(
                    e -> componentOperation.requestDeleting(binder.getBean(), this, grid));
            addButtons(requestDeleting);
        }
        if (dto.getStatus() == VirtualServerStatus.DELETING && isInf) {
            Button cancelDeleting = VaadinComponents.getButton(
                    Transl.get(VirtualServerAction.CANCEL_DELETING.getAction()), VaadinIcon.RECYCLE.create());
            cancelDeleting.addClickListener(e -> componentOperation.cancelDeleting(binder.getBean(), this, grid));
            addButtons(cancelDeleting);
        }
        if (dto.getStatus() == VirtualServerStatus.CREATING && isInf) {
            Button submit = VaadinComponents.getSubmitButton();
            submit.setText(Transl.get(VirtualServerAction.CREATE.getAction()));
            submit.addClickListener(e -> componentOperation.create(
                    binder, hddList, binderList, this, grid));
            addButtons(submit);
        }
        if (dto.getStatus() == VirtualServerStatus.DELETING && isInf) {
            Button submit = VaadinComponents.getSubmitButton();
            submit.setText(VirtualServerAction.DELETE.getAction());
            submit.addClickListener(e -> componentOperation.delete(binder.getBean(), this, grid));
            addButtons(submit);
        }
        if (dto.getId() == null || (dto.getStatus() == VirtualServerStatus.RUNNING && isInf)) {
            Button submit = VaadinComponents.getSubmitButton();
            submit.addClickListener(e -> componentOperation.save(
                    binder, hddList, binderList, VirtualServerStatus.CREATING, this, grid));
            addSubmitButton(submit);
        }
    }
}
