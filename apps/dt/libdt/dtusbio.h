#ifndef DTUSBIO_H
#define DTUSBIO_H

dt_device
DT_LIB_API
dt_usb_device_open(char *deviceName);

int
DT_LIB_API
dt_usb_device_close(dt_device device);

dt_io_error
DT_LIB_API
dt_usb_device_read(dt_device h, dt_table *tp, dt_frame *fpa[]);

int
DT_LIB_API
dt_usb_table_init(dt_table *tp);

#endif /* DTUSBIO_H */
