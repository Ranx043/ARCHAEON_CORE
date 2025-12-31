---
title: "ARSENAL EMULADORES - Emuladores y Virtualizacion Legacy"
category: "ARCHAEON_ARSENAL"
type: "tool_documentation"
version: "1.0.0"
created: "2025-12-31"
tools: ["QEMU", "DOSBox", "Hercules", "VICE", "MAME"]
purpose: "Documentación de emuladores para sistemas y arquitecturas legacy"
tags: ["emulators", "qemu", "dosbox", "mainframe", "retro", "virtualization"]
---

# ARSENAL EMULADORES

## Guía Completa de Emuladores para Sistemas Legacy

Este documento cubre la instalación, configuración y uso de emuladores
para ejecutar software legacy en arquitecturas antiguas y sistemas operativos históricos.

---

## 1. QEMU

### 1.1 Descripción

QEMU (Quick Emulator) es un emulador y virtualizador de código abierto
que soporta múltiples arquitecturas incluyendo x86, ARM, MIPS, PowerPC, y más.

### 1.2 Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install qemu-system-x86 qemu-system-arm qemu-system-misc
sudo apt install qemu-user qemu-user-static

# Linux (Fedora/RHEL)
sudo dnf install qemu-system-x86 qemu-system-arm
sudo dnf install qemu-user qemu-user-static

# macOS
brew install qemu

# Windows
# Descargar de: https://www.qemu.org/download/#windows
# O via Chocolatey:
choco install qemu
```

### 1.3 Modos de Operación

#### System Mode (Emulación Completa)

Emula un sistema completo: CPU, memoria, dispositivos.

```bash
# Crear imagen de disco
qemu-img create -f qcow2 disk.qcow2 20G

# Instalar SO desde ISO
qemu-system-x86_64 \
    -m 2048 \
    -cdrom os.iso \
    -hda disk.qcow2 \
    -boot d

# Ejecutar sistema instalado
qemu-system-x86_64 \
    -m 2048 \
    -hda disk.qcow2 \
    -enable-kvm
```

#### User Mode (Emulación de Usuario)

Ejecuta binarios de otras arquitecturas directamente.

```bash
# Ejecutar binario ARM en x86
qemu-arm ./program_arm

# Ejecutar binario MIPS
qemu-mips ./program_mips

# Con bibliotecas específicas
qemu-arm -L /usr/arm-linux-gnueabihf ./program_arm
```

### 1.4 Emulación x86

```bash
# x86 32-bit
qemu-system-i386 \
    -m 512 \
    -hda disk.img \
    -soundhw sb16 \
    -vga std

# x86 64-bit con KVM (aceleración)
qemu-system-x86_64 \
    -m 4096 \
    -smp 2 \
    -hda disk.qcow2 \
    -enable-kvm \
    -cpu host

# BIOS/UEFI
qemu-system-x86_64 \
    -bios /usr/share/ovmf/OVMF.fd \
    -m 2048 \
    -hda disk.qcow2
```

### 1.5 Emulación ARM

```bash
# ARM 32-bit (versatilepb)
qemu-system-arm \
    -M versatilepb \
    -m 256 \
    -kernel zImage \
    -dtb versatile-pb.dtb \
    -initrd initramfs.cpio.gz \
    -append "console=ttyAMA0" \
    -nographic

# ARM 64-bit (virt)
qemu-system-aarch64 \
    -M virt \
    -cpu cortex-a57 \
    -m 2048 \
    -kernel Image \
    -initrd initramfs.cpio.gz \
    -nographic

# Raspberry Pi emulation
qemu-system-arm \
    -M raspi2b \
    -kernel kernel7.img \
    -dtb bcm2709-rpi-2-b.dtb \
    -sd raspbian.img \
    -append "console=ttyAMA0 root=/dev/mmcblk0p2" \
    -serial stdio
```

### 1.6 Networking

```bash
# User-mode networking (NAT)
qemu-system-x86_64 \
    -netdev user,id=net0,hostfwd=tcp::2222-:22 \
    -device e1000,netdev=net0

# Bridge networking
sudo qemu-system-x86_64 \
    -netdev bridge,id=net0,br=br0 \
    -device virtio-net-pci,netdev=net0

# Shared folder (9p)
qemu-system-x86_64 \
    -virtfs local,path=/shared,mount_tag=host0,security_model=mapped-xattr
```

### 1.7 Opciones Comunes

```bash
# Memoria y CPU
-m 2048                     # 2GB RAM
-smp 4                      # 4 CPUs virtuales
-cpu host                   # Usar CPU del host (con KVM)
-cpu qemu64                 # CPU emulada genérica

# Almacenamiento
-hda disk.img               # Disco primario
-hdb disk2.img              # Disco secundario
-cdrom image.iso            # CD-ROM
-drive file=disk.qcow2,format=qcow2

# Display
-display gtk                # Ventana GTK
-display sdl                # Ventana SDL
-nographic                  # Sin gráficos (serial)
-vnc :1                     # VNC en :1 (puerto 5901)
-spice port=5900            # SPICE

# Boot
-boot d                     # Boot desde CD
-boot c                     # Boot desde disco
-boot order=cdn             # Orden: CD, disco, network

# Otros
-snapshot                   # No guardar cambios
-daemonize                  # Ejecutar en background
-monitor stdio              # Monitor QEMU en terminal
```

### 1.8 QEMU Monitor

```
# Acceder al monitor
Ctrl+Alt+2                  # En ventana gráfica
# O con -monitor stdio

# Comandos del monitor
(qemu) info status          # Estado de la VM
(qemu) info cpus            # Info de CPUs
(qemu) info network         # Info de red
(qemu) info block           # Info de discos
(qemu) info snapshots       # Listar snapshots

(qemu) savevm nombre        # Crear snapshot
(qemu) loadvm nombre        # Cargar snapshot
(qemu) delvm nombre         # Eliminar snapshot

(qemu) stop                 # Pausar VM
(qemu) cont                 # Continuar VM
(qemu) quit                 # Salir

(qemu) sendkey ctrl-alt-del # Enviar teclas
(qemu) screendump file.ppm  # Captura de pantalla
```

---

## 2. DOSBox

### 2.1 Descripción

DOSBox emula un PC compatible IBM con DOS para ejecutar
programas y juegos MS-DOS.

### 2.2 Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install dosbox

# También DOSBox-X (fork mejorado)
sudo apt install dosbox-x

# macOS
brew install dosbox
brew install dosbox-x

# Windows
# Descargar de: https://www.dosbox.com/
# O via Chocolatey:
choco install dosbox
```

### 2.3 Uso Básico

```bash
# Iniciar DOSBox
dosbox

# Iniciar con programa específico
dosbox programa.exe

# Iniciar con archivo de configuración
dosbox -conf mi_config.conf
```

### 2.4 Comandos Internos DOSBox

```dos
# Montar unidades
MOUNT C ~/dosgames
MOUNT D /path/to/cdrom -t cdrom
MOUNT A /path/to/floppy -t floppy

# Imagen de disco
IMGMOUNT D game.iso -t iso
IMGMOUNT A floppy.img -t floppy
IMGMOUNT 2 hdd.img -size 512,63,16,520

# Cambiar de unidad
C:
D:

# Listar archivos
DIR
DIR /P
DIR /W

# Ejecutar programa
PROGRAMA.EXE
INSTALL.EXE
```

### 2.5 Configuración

Archivo de configuración: `~/.dosbox/dosbox-0.74.conf` (Linux) o
`~\AppData\Local\DOSBox\dosbox-0.74.conf` (Windows).

```ini
[sdl]
fullscreen=false
fulldouble=false
output=opengl
windowresolution=800x600

[dosbox]
machine=svga_s3
memsize=16

[cpu]
core=auto
cputype=auto
cycles=auto
# cycles=fixed 10000    # Para juegos antiguos

[mixer]
rate=44100
blocksize=1024
prebuffer=20

[sblaster]
sbtype=sb16
sbbase=220
irq=7
dma=1
hdma=5

[gus]
gus=false

[speaker]
pcspeaker=true
pcrate=44100

[serial]
serial1=disabled

[dos]
xms=true
ems=true
umb=true

[autoexec]
# Comandos automáticos al iniciar
MOUNT C ~/dosgames
C:
```

### 2.6 Teclas Especiales

```
Alt+Enter           - Pantalla completa
Ctrl+F1             - Keymapper
Ctrl+F4             - Cambiar imagen montada
Ctrl+F5             - Captura de pantalla
Ctrl+Alt+F5         - Grabar video
Ctrl+F6             - Grabar audio
Ctrl+F7             - Disminuir frameskip
Ctrl+F8             - Aumentar frameskip
Ctrl+F9             - Cerrar DOSBox
Ctrl+F10            - Capturar/liberar ratón
Ctrl+F11            - Disminuir ciclos
Ctrl+F12            - Aumentar ciclos
```

### 2.7 DOSBox-X

Fork de DOSBox con características adicionales:

```bash
# Características adicionales:
# - Soporte Windows 3.x/9x mejor
# - Emulación PC-98
# - Mejor soporte de hardware
# - Integración con clipboard

dosbox-x
dosbox-x -conf config.conf
```

---

## 3. HERCULES (Mainframe Emulator)

### 3.1 Descripción

Hercules emula mainframes IBM System/370, ESA/390 y z/Architecture
para ejecutar sistemas operativos mainframe como MVS, VM/CMS, z/OS.

### 3.2 Instalación

```bash
# Linux (Debian/Ubuntu)
sudo apt install hercules

# Linux (desde fuentes)
git clone https://github.com/SDL-Hercules-390/hyperion.git
cd hyperion
./configure
make
sudo make install

# Windows
# Descargar de: http://www.hercules-390.org/

# Verificar
hercules --version
```

### 3.3 Configuración Básica

Archivo `hercules.cnf`:

```ini
# Hercules Configuration File

# CPU
CPUSERIAL 002623
CPUMODEL  3090
MAINSIZE  64
XPNDSIZE  0
NUMCPU    1
ARCHMODE  ESA/390

# Consoles
CNSLPORT  3270

# Devices
# Console
0700 3270
# Card Reader
000C 3505 reader.txt
# Card Punch
000D 3525 punch.txt
# Printer
000E 1403 printer.txt
# Tape
0480 3420 tape1.aws
# DASD (Disk)
0120 3390 volume1.cckd

# Network (optional)
0E20 LCS -n tap0

# Panel display
PANRATE SLOW
```

### 3.4 Uso Básico

```bash
# Iniciar Hercules
hercules -f hercules.cnf

# Comandos de consola Hercules
# (en la consola de Hercules, prefijo con /)

/ipl 0120                  # IPL desde dispositivo 0120
/cpu 0                     # Seleccionar CPU 0
/start                     # Iniciar CPU
/stop                      # Detener CPU
/quit                      # Salir de Hercules
/devlist                   # Listar dispositivos
/attach 0480 3420 tape.aws # Conectar cinta
/detach 0480               # Desconectar dispositivo
```

### 3.5 Crear DASD (Discos)

```bash
# Crear volumen 3390 (modelo 3)
dasdinit -z volume.cckd 3390-3 VOL001

# Crear volumen comprimido
dasdinit -bz volume.cckd 3390-3 VOL001

# Otros tipos
dasdinit disk.cckd 3380-K DISK01    # 3380
dasdinit disk.cckd 3350 DISK02      # 3350
```

### 3.6 Terminal 3270

```bash
# Instalar emulador 3270
sudo apt install x3270 c3270

# Conectar a Hercules
c3270 localhost:3270       # Terminal de texto
x3270 localhost:3270       # Terminal gráfico
```

### 3.7 Ejecutar MVS/TK (MVS Turnkey)

Sistema MVS 3.8j pre-configurado:

```bash
# Descargar MVS/TK
# http://www.bsp-gmbh.com/turnkey/

# Iniciar
cd mvstk
./startmvs

# Conectar con terminal 3270
x3270 localhost:3270
```

---

## 4. EMULADORES RETRO

### 4.1 VICE (Commodore 64/128)

```bash
# Instalación
sudo apt install vice

# Ejecutar C64
x64                        # C64
x64sc                      # C64 (cycle-exact)
x128                       # C128
xvic                       # VIC-20
xplus4                     # Plus/4
xpet                       # PET

# Cargar programa
x64 programa.prg
x64 juego.d64             # Imagen de disco

# Atajos
Alt+D           # Abrir imagen de disco
Alt+8           # Attach disk image drive 8
Alt+J           # Joystick settings
Alt+M           # Monitor/Debugger
F9              # Reset
F12             # Settings
```

### 4.2 MAME (Multiple Arcade Machine Emulator)

```bash
# Instalación
sudo apt install mame

# Ejecutar
mame                      # Frontend
mame romname              # ROM específica
mame -listxml             # Listar info XML
mame -verifyroms          # Verificar ROMs

# Configurar paths
# ~/.mame/mame.ini
rompath                 /home/user/roms
```

### 4.3 Otros Emuladores Retro

```bash
# Apple II
sudo apt install linapple

# Atari 800/XL/XE
sudo apt install atari800

# ZX Spectrum
sudo apt install fuse-emulator-gtk

# Amiga
sudo apt install fs-uae

# MSX
sudo apt install openmsx

# NES/SNES
sudo apt install fceux              # NES
sudo apt install snes9x-gtk         # SNES
```

---

## 5. VIRTUAL MACHINES PARA LEGACY OS

### 5.1 VirtualBox

```bash
# Instalación
sudo apt install virtualbox

# Crear VM desde línea de comandos
VBoxManage createvm --name "DOS622" --ostype DOS --register
VBoxManage modifyvm "DOS622" --memory 32 --vram 8
VBoxManage createhd --filename DOS622.vdi --size 500
VBoxManage storagectl "DOS622" --name "IDE" --add ide
VBoxManage storageattach "DOS622" --storagectl "IDE" \
    --port 0 --device 0 --type hdd --medium DOS622.vdi

# Configuraciones para OS legacy
VBoxManage modifyvm "DOS622" --biosbootmenu disabled
VBoxManage modifyvm "DOS622" --audio sb16
VBoxManage modifyvm "DOS622" --graphicscontroller vboxvga
```

### 5.2 VMware

Configuración para sistemas legacy (.vmx):

```
# Config para DOS/Windows 3.1
guestOS = "dos"
memsize = "32"
sound.present = "TRUE"
sound.virtualDev = "sb16"

# Config para Windows 95
guestOS = "win95"
memsize = "64"
```

### 5.3 86Box/PCem

Emuladores de PC vintage con precisión de hardware:

```bash
# 86Box
# Descargar de: https://86box.net/

# Características:
# - Emulación precisa de chipsets
# - Soporte de ISA/VLB/PCI
# - Sound Blaster, Roland MT-32
# - BIOS reales
```

---

## 6. CONFIGURACIONES ESPECÍFICAS

### 6.1 Ejecutar COBOL en Hercules/MVS

```jcl
//COBOLJOB JOB (ACCT),'COBOL COMPILE',CLASS=A
//STEP1    EXEC IGYWCL
//COBOL.SYSIN DD *
       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY "HELLO FROM MVS".
           STOP RUN.
/*
```

### 6.2 Testing DOS Assembly

```bash
# Con DOSBox
dosbox

# En DOSBox
MOUNT C ~/projects
C:
MASM PROGRAM.ASM;
LINK PROGRAM.OBJ;
PROGRAM.EXE
```

### 6.3 Windows 3.1 en QEMU

```bash
# Crear disco
qemu-img create -f qcow2 win31.qcow2 500M

# Instalar
qemu-system-i386 \
    -m 32 \
    -hda win31.qcow2 \
    -fda dos622.img \
    -boot a \
    -soundhw sb16 \
    -vga cirrus

# Ejecutar
qemu-system-i386 \
    -m 32 \
    -hda win31.qcow2 \
    -soundhw sb16 \
    -vga cirrus
```

---

## 7. TIPS Y TROUBLESHOOTING

### 7.1 Performance

```bash
# QEMU con KVM (Linux)
# Verificar soporte
egrep -c '(vmx|svm)' /proc/cpuinfo
# Debe ser > 0

# Cargar módulo
sudo modprobe kvm
sudo modprobe kvm_intel  # o kvm_amd

# Permisos
sudo usermod -aG kvm $USER
```

### 7.2 Audio en DOSBox

```ini
# Si no hay sonido, verificar config
[sblaster]
sbtype=sb16
sbbase=220
irq=7
dma=1
hdma=5
sbmixer=true
oplmode=auto
oplrate=44100
```

### 7.3 Networking en QEMU

```bash
# Si no funciona networking
# Verificar permisos bridge
sudo chmod 644 /etc/qemu/bridge.conf
# Contenido: allow br0

# User-mode networking simple
-nic user,model=virtio
```

### 7.4 Imágenes de Disco

```bash
# Convertir formatos QEMU
qemu-img convert -f raw -O qcow2 disk.img disk.qcow2
qemu-img convert -f vmdk -O qcow2 disk.vmdk disk.qcow2

# Info de imagen
qemu-img info disk.qcow2

# Redimensionar
qemu-img resize disk.qcow2 +10G
```

---

## 8. RECURSOS Y REFERENCIAS

### 8.1 Sistemas Operativos Legacy

- MS-DOS: WinWorld (https://winworldpc.com/)
- MVS 3.8j: http://www.bsp-gmbh.com/turnkey/
- CP/M: http://www.cpm.z80.de/
- FreeDOS: https://www.freedos.org/

### 8.2 Documentación

- QEMU: https://www.qemu.org/documentation/
- DOSBox Wiki: https://www.dosbox.com/wiki/
- Hercules: http://www.hercules-390.org/
- VICE: https://vice-emu.sourceforge.io/

### 8.3 ROMs y BIOS

- ROMs legales (abandonware)
- BIOS de dominio público
- Sistemas operativos libres (FreeDOS, etc.)

---

*Última actualización: 2025-12-31*
*ARCHAEON Arsenal - Emuladores Legacy*
